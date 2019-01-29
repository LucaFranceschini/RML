package rml.ast

import prolog.ast.*

// translates from RML AST to Prolog AST

fun toProlog(spec: Specification): LogicProgram {
    // predicates to be exported
    val traceExpressionIndicator = PredicateIndicatorTerm("trace_expression", 2)
    val matchIndicator = PredicateIndicatorTerm("match", 2)
    val exportedPredicates = ListTerm(listOf(traceExpressionIndicator, matchIndicator))

    // export as a module
    val moduleDeclaration = Directive(Atom("module", ConstantTerm("spec"), exportedPredicates))

    // import deep_subdict
    val deepSubdictImport = Directive(Atom("use_module",
            FunctionTerm("monitor", ConstantTerm("deep_subdict"))))

    // generate a match clause for each event type pattern
    val matchClauses = spec.evtypeDecls.map(::toProlog).flatten()
    val anyMatchClause = Clause(Atom("match", VarTerm("_"), FunctionTerm("any")))
    val traceExpClause = toProlog(spec.traceExpDecls)

    // spread operator can only be applied to arrays
    return LogicProgram(listOf(moduleDeclaration, deepSubdictImport),
            matchClauses + anyMatchClause + traceExpClause)
}

// build a match clause for each pattern if positive, or just one if negated
fun toProlog(evtypeDecl: EvtypeDecl): List<Clause> {
    val eventDict = VarTerm("Event")

    // unfold patterns and collect atoms
    val patternAtoms: List<Atom> = when (evtypeDecl) {
        is DirectEvtypeDecl -> evtypeDecl.patternValue.unfoldOrPatterns().map {
            Atom("deep_subdict", toProlog(it, isMatchClause = true), eventDict)
        }
        is DerivedEvtypeDecl -> evtypeDecl.parents.map {
            Atom("match", eventDict, toProlog(it))
        }
    }

    // match clause head is always the same
    val head = Atom("match", eventDict, toProlog(evtypeDecl.evtype, isMatchClause = true))

    // if it's negated just make one match clause, applying negation to all body atoms
    if (evtypeDecl.negated) return listOf(Clause(
            head,
            patternAtoms.map(Atom::negate)
    ))

    // if it's positive produce a match clause for each pattern
    return patternAtoms.map { Clause(head, listOf(it)) }
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

fun toProlog(declarations: List<TraceExpDecl>): Clause {
    val head = Atom("trace_expression", ConstantTerm("Main"), VarTerm("Main"))
    val body = declarations.map(::toProlog)
    return Clause(head, body)
}

// output T = trace-expression
fun toProlog(declaration: TraceExpDecl): Atom {
    if (declaration.vars.size > 1)
        throw Exception("multiple generics not yet supported")

    val varTerm = VarTerm(declaration.id.name)
    var body = toProlog(declaration.traceExp, outsideConcatenation = true)

    if (declaration.vars.isNotEmpty()) {
        val paramName = FunctionTerm(declaration.vars.first().name)
        body = FunctionTerm("gen", paramName, body)
    }

    return Atom("=", varTerm, body)
}

fun toProlog(traceExp: TraceExp, outsideConcatenation: Boolean = false): PrologTerm = when(traceExp) {
    EmptyTraceExp -> FunctionTerm("eps")
    NoneTraceExp -> IntTerm(0)
    AnyTraceExp -> toProlog(EventTypeTraceExp(EventType("any", emptyList())), outsideConcatenation)
    AllTraceExp -> FunctionTerm("star", toProlog(AnyTraceExp, outsideConcatenation = true))
    is ClosureTraceExp -> FunctionTerm("clos", toProlog(traceExp.exp, outsideConcatenation = true))
    is BlockTraceExp -> toProlog(traceExp, outsideConcatenation)
    is TraceExpVar -> toProlog(traceExp)
    is EventTypeTraceExp ->
        if (outsideConcatenation) FunctionTerm(":", toProlog(traceExp.eventType), toProlog(EmptyTraceExp))
        else toProlog(traceExp.eventType)
    is ConcatTraceExp -> toProlog(traceExp)
    is AndTraceExp -> toProlog(traceExp, "/\\", outsideConcatenation)
    is OrTraceExp -> toProlog(traceExp, "\\/", outsideConcatenation)
    is ShuffleTraceExp -> toProlog(traceExp, "|", outsideConcatenation)
    is FilterTraceExp -> FunctionTerm(";",
            FunctionTerm(">>",
                    toProlog(traceExp.evtype), toProlog(traceExp.leftExp, outsideConcatenation)),
            toProlog(traceExp.rightExp, outsideConcatenation))
    is CondFilterTraceExp -> FunctionTerm(";",
            FunctionTerm(">",
                    toProlog(traceExp.evtype), toProlog(traceExp.leftExp, outsideConcatenation)),
            toProlog(traceExp.rightExp, outsideConcatenation))
    is StarTraceExp -> FunctionTerm("star", toProlog(traceExp.eventType))
    is PlusTraceExp -> FunctionTerm("plus", toProlog(traceExp.eventType))
    is OptionalTraceExp -> FunctionTerm("optional", toProlog(traceExp.eventType))
}

fun toProlog(block: BlockTraceExp, outsideConcatenation: Boolean): PrologTerm {
    // handle one variable at a time
    val firstVar = block.declaredVars.first()
    val otherVars = block.declaredVars.drop(1)
    val innerBlock: PrologTerm =
            if (otherVars.isEmpty()) toProlog(block.traceExp, outsideConcatenation)
            else toProlog(BlockTraceExp(otherVars, block.traceExp), outsideConcatenation)
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

fun toProlog(traceExp: BinaryTraceExp, opSymbol: String, outsideConcatenation: Boolean) = FunctionTerm(
        opSymbol,
        toProlog(traceExp.left, outsideConcatenation),
        toProlog(traceExp.right, outsideConcatenation))

fun toProlog(traceExp: TraceExpVar): PrologTerm {
    val variable = VarTerm(traceExp.id.name)

    if (traceExp.genericArgs.isEmpty())
        return variable

    if (traceExp.genericArgs.size > 1)
        throw Exception("multiple generics not supported yet")

    return FunctionTerm("app",
            variable,
            FunctionTerm("var",
                    FunctionTerm(traceExp.genericArgs.first().name)))
}

// isMatchClause true when generating match clauses
fun toProlog(value: SimpleValue, isMatchClause: Boolean = false): PrologTerm = when(value) {
    is VarValue ->
        if (isMatchClause) VarTerm(toValidPrologVarName(value.id.name))
        else FunctionTerm("var", FunctionTerm(value.id.name))
    is IntValue -> IntTerm(value.number)
    is BooleanValue -> ConstantTerm(value.boolean.toString())
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