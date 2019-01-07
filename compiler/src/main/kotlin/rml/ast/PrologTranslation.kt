package rml.ast

import prolog.ast.*

// translates from RML AST to Prolog AST

fun toProlog(spec: Specification): LogicProgram {
    // generate a match clause for each event type pattern
    val matchClauses = spec.evtypeDecls.map(::toProlog).flatten()
    val traceExpClause = toProlog(spec.traceExpDecls, spec.mainTraceExp)

    // spread operator can only be applied to arrays
    return LogicProgram(*matchClauses.toTypedArray(), traceExpClause)
}

// build match clauses
fun toProlog(evtypeDecl: EvtypeDecl): List<Clause> {
    // match(E, eventType(...))
    val eventVar = VarTerm("E")
    val eventType = toProlog(evtypeDecl.evtype)
    val head = Atom("match", eventVar, eventType)

    // generate a clause for each pattern
    return when (evtypeDecl) {
        is DirectEvtypeDecl -> {
            evtypeDecl.objects.map {
                val body = toProlog(it, eventVar)
                Clause(head, body)
            }.toList()
        }
        is DerivedEvtypeDecl -> {
            evtypeDecl.parents.map {
                val bodyAtom = Atom("match", eventVar, toProlog(it))
                Clause(head, bodyAtom)
            }.toList()
        }
    }
}

object UniqueVarGenerator {
    private var id = 1

    fun get() = VarTerm("UNIQUE_VAR${id++}")
}

// convert object to list of dictionary accesses
fun toProlog(objectValue: ObjectValue, dict: PrologTerm): List<Atom> {
    val result = mutableListOf<Atom>()

    for (field in objectValue.fields) {
        val key = ConstantTerm(field.key.name)

        when(field.value) {
            // if it is a primitive value just add the access
            is SimpleValue -> {
                val value = toProlog(field.value)
                result.add(Atom("get_dict", key, dict, value))
            }
            is ObjectValue -> {
                // retrieve inner dict
                val innerDict = UniqueVarGenerator.get()
                result.add(Atom("get_dict", key, dict, innerDict))
                // recursively add all the accesses from the inner dict
                result.addAll(toProlog(field.value, innerDict))
            }
        }
    }

    return result
}

fun toProlog(declarations: List<TraceExpDecl>, mainTraceExp: TraceExpId): Clause {
    val head = Atom("trace_expression", ConstantTerm(mainTraceExp.name), VarTerm(mainTraceExp.name))
    val body = declarations.map(::toProlog)
    return Clause(head, body)
}

// output T = trace-expression
fun toProlog(declaration: TraceExpDecl): Atom = Atom(
        "=",
        VarTerm(declaration.id.name),
        toProlog(declaration.traceExp))

fun toProlog(traceExp: TraceExp): PrologTerm = when(traceExp) {
    EmptyTraceExp -> FunctionTerm("eps")
    is BlockTraceExp -> FunctionTerm("var",
            ListTerm(traceExp.declaredVars.map { ConstantTerm(it.name) }.toList()),
            toProlog(traceExp.traceExp))
    is TraceExpVar -> toProlog(traceExp)
    is EventTypeTraceExp -> FunctionTerm(traceExp.id.name, traceExp.dataValues.map { toProlog(it) }.toList())
    is ConcatTraceExp -> toProlog(traceExp, "*")
    is AndTraceExp -> toProlog(traceExp, "/\\")
    is OrTraceExp -> toProlog(traceExp, "\\/")
    is ShuffleTraceExp -> toProlog(traceExp, "|")
}

fun toProlog(traceExp: BinaryTraceExp, opSymbol: String): PrologTerm =
        FunctionTerm(opSymbol, toProlog(traceExp.left), toProlog(traceExp.right))

fun toProlog(traceExp: TraceExpVar): PrologTerm {
    val variable = VarTerm(traceExp.id.name)

    if (traceExp.genericVars.isEmpty())
        return variable

    return FunctionTerm("app", variable,
            ListTerm(traceExp.genericVars.map { ConstantTerm(it.name) }.toList()))
}

fun toProlog(value: SimpleValue): PrologTerm = when(value) {
    is VarValue -> FunctionTerm("var", ConstantTerm(value.id.name))
    is IntValue -> IntTerm(value.number)
    is StringValue -> ConstantTerm(value.string)
}