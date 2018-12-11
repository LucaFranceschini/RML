package rml.ast

import prolog.ast.*

// translates from RML AST to Prolog AST

fun toProlog(spec: Specification, id: String) = LogicProgram(Clause(
        Atom("trace_expression", FunctionTerm.constant(id), VarTerm(spec.mainTraceExp.name)), // head
        spec.declarations.map { toProlog(it) }
))

// output T = trace-expression
fun toProlog(declaration: Declaration): Atom = Atom(
        "=",
        VarTerm(declaration.id.name),
        toProlog(declaration.traceExp))

fun toProlog(traceExp: TraceExp): PrologTerm = when(traceExp) {
    EmptyTraceExp -> FunctionTerm("eps")
    is BlockTraceExp -> FunctionTerm("var",
            ListTerm(traceExp.declaredVars.map { FunctionTerm.constant(it.name) }.toList()),
            toProlog(traceExp.traceExp))
    is TraceExpVar -> toProlog(traceExp)
    is EventTypeTraceExp -> FunctionTerm(traceExp.id.name, traceExp.eventTerms.map { toProlog(it) }.toList())
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
            ListTerm(traceExp.genericVars.map { FunctionTerm.constant(it.name) }.toList()))
}

fun toProlog(eventTerm: EventTerm): PrologTerm = when(eventTerm) {
    is VarEventTerm -> FunctionTerm("var", FunctionTerm.constant(eventTerm.id.name))
    is IntEventTerm -> IntTerm(eventTerm.number)
    is StringEventTerm -> FunctionTerm.constant(eventTerm.string)
}