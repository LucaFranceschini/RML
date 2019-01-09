package rml.ast

import prolog.ast.*

// translates from RML AST to Prolog AST

fun toProlog(spec: Specification): LogicProgram {
    // export as a module
    val moduleDeclaration = Directive(Atom("module", ConstantTerm("spec"), ListTerm(
            PredicateIndicatorTerm("trace_expression", 2),
            PredicateIndicatorTerm("match", 2)
    )))

    // import deep_subdict
    val deepSubdictImport = Directive(Atom("use_module",
            FunctionTerm("monitor", ConstantTerm("deep_subdict"))))

    // generate a match clause for each event type pattern
    val matchClauses = spec.evtypeDecls.map(::toProlog).flatten()
    val traceExpClause = toProlog(spec.traceExpDecls, spec.mainTraceExp)

    // spread operator can only be applied to arrays
    return LogicProgram(listOf(moduleDeclaration, deepSubdictImport),
            matchClauses + traceExpClause)
}

// build a match clause for each pattern
fun toProlog(evtypeDecl: EvtypeDecl): List<Clause> {
    val eventDict = VarTerm("Event")

    return when (evtypeDecl) {
        is DirectEvtypeDecl -> evtypeDecl.patternValue.unfoldOrPatterns().map {
            Clause(
                    Atom("match", eventDict, toProlog(evtypeDecl.evtype, isMatchClause = true)),
                    // the dictionary specified by the pattern must be a sub-dictionary of the observed event
                    Atom("deep_subdict", toProlog(it, isMatchClause = true), eventDict))
        }
        is DerivedEvtypeDecl -> evtypeDecl.parents.map {
            Clause(
                    Atom("match", eventDict, toProlog(evtypeDecl.evtype)),
                    Atom("match", eventDict, toProlog(it)))
        }
    }
}

fun toProlog(dataValue: DataValue, isMatchClause: Boolean = false): PrologTerm = when (dataValue) {
    is ObjectValue -> DictionaryTerm.from(
            dataValue.fields.map { Pair(it.key.name, toProlog(it.value, isMatchClause)) }.toMap())
    is ListValue -> ListTerm(
            dataValue.values.map { toProlog(it, isMatchClause) },
            if (dataValue.hasEllipsis) VarTerm("_") else null)
    is SimpleValue -> toProlog(dataValue, isMatchClause)
    is OrPatternValue -> throw Exception("internal error: or-patterns should be unfolded by now")
}

fun toProlog(declarations: List<TraceExpDecl>, mainTraceExp: TraceExpId): Clause {
    val head = Atom("trace_expression", ConstantTerm(mainTraceExp.name), VarTerm(mainTraceExp.name))
    val body = declarations.map(::toProlog)
    return Clause(head, body)
}

// output T = trace-expression
fun toProlog(declaration: TraceExpDecl): Atom {
    if (declaration.vars.size > 1)
        throw Exception("multiple generics not yet supported")

    val varTerm = VarTerm(declaration.id.name)
    var body = toProlog(declaration.traceExp, topLevel = true)

    if (declaration.vars.isNotEmpty()) {
        val paramName = FunctionTerm(declaration.vars.first().name)
        body = FunctionTerm("gen", paramName, body)
    }

    return Atom("=", varTerm, body)
}

fun toProlog(traceExp: TraceExp, topLevel: Boolean = false): PrologTerm = when(traceExp) {
    EmptyTraceExp -> FunctionTerm("eps")
    is BlockTraceExp -> toProlog(traceExp)
    is TraceExpVar -> toProlog(traceExp)
    is EventTypeTraceExp ->
        if (topLevel) FunctionTerm(":", toProlog(traceExp.eventType), toProlog(EmptyTraceExp))
        else toProlog(traceExp.eventType)
    is ConcatTraceExp -> toProlog(traceExp)
    is AndTraceExp -> toProlog(traceExp, "/\\")
    is OrTraceExp -> toProlog(traceExp, "\\/")
    is ShuffleTraceExp -> toProlog(traceExp, "|")
    is FilterTraceExp -> FunctionTerm(">>",
            toProlog(traceExp.evtype),
            toProlog(traceExp.traceExp))
}

fun toProlog(block: BlockTraceExp): PrologTerm {
    // handle one variable at a time
    val firstVar = block.declaredVars.first()
    val otherVars = block.declaredVars.drop(1)
    val innerBlock: PrologTerm =
            if (otherVars.isEmpty()) toProlog(block.traceExp)
            else toProlog(BlockTraceExp(otherVars, block.traceExp))
    return FunctionTerm("var", FunctionTerm(firstVar.name), innerBlock)
}

fun toProlog(concat: ConcatTraceExp): FunctionTerm {
    val left = concat.left
    val right = concat.right

    if (left is EventTypeTraceExp && right is EventTypeTraceExp)
        // left:(right:eps)
        return FunctionTerm(":",
                toProlog(left),
                FunctionTerm(":",
                        toProlog(right),
                        toProlog(EmptyTraceExp)))

    if (left is EventTypeTraceExp && right !is EventTypeTraceExp)
        // left:right
        return FunctionTerm(":",
                toProlog(left),
                toProlog(right))

    if (left !is EventTypeTraceExp && right is EventTypeTraceExp)
        // left*(right:eps)
        return FunctionTerm("*",
                toProlog(left),
                FunctionTerm(":",
                        toProlog(right),
                        toProlog(EmptyTraceExp)))

    // both left and right not event types
    return FunctionTerm("*", toProlog(left), toProlog(right))
}

fun toProlog(eventType: EventType, isMatchClause: Boolean = false) = FunctionTerm(
        eventType.id.name,
        eventType.dataValues.map { toProlog(it, isMatchClause) }.toList())

fun toProlog(traceExp: BinaryTraceExp, opSymbol: String) =
        FunctionTerm(opSymbol, toProlog(traceExp.left), toProlog(traceExp.right))

fun toProlog(traceExp: TraceExpVar): PrologTerm {
    val variable = VarTerm(traceExp.id.name)

    if (traceExp.genericVars.isEmpty())
        return variable

    if (traceExp.genericVars.size > 1)
        throw Exception("multiple generics not supported yet")

    return FunctionTerm("app",
            variable,
            FunctionTerm("var",
                    FunctionTerm(traceExp.genericVars.first().name)))
}

// isMatchClause true when generating match clauses
fun toProlog(value: SimpleValue, isMatchClause: Boolean = false): PrologTerm = when(value) {
    is VarValue ->
        if (isMatchClause) VarTerm(toValidPrologVarName(value.id.name))
        else FunctionTerm("var", FunctionTerm(value.id.name))
    is IntValue -> IntTerm(value.number)
    is StringValue -> StringTerm(value.string)
    is ListSimpleValue -> ListTerm(
            value.values.map { toProlog(it) },
            if (value.hasEllipsis) VarTerm("_") else null)
    is UnusedValue -> VarTerm("_")
}

fun toValidPrologVarName(string: String): String {
    if (string.startsWith("_"))
        return string

    val first = string.first()
    if (first.isLetter())
        return string.replaceFirst(first, first.toUpperCase())

    throw Exception("invalid Prolog variable name, can't fix")
}