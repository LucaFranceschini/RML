package rml.ast

sealed class TraceExp

abstract class BinaryTraceExp: TraceExp() {
    // declare it abstract and override it in subclasses to avoid duplicating the field
    // (can't define the property here because data classes need to define at least one property)
    abstract val left: TraceExp
    abstract val right: TraceExp
}

data class ConcatTraceExp(override val left: TraceExp, override val right: TraceExp): BinaryTraceExp()
data class AndTraceExp(override val left: TraceExp, override val right: TraceExp): BinaryTraceExp()
data class OrTraceExp(override val left: TraceExp, override val right: TraceExp): BinaryTraceExp()
data class ShuffleTraceExp(override val left: TraceExp, override val right: TraceExp): BinaryTraceExp()

object EmptyTraceExp: TraceExp()

// scoped declaration of one or more variables
data class BlockTraceExp(val declaredVars: List<VarId>, val traceExp: TraceExp): TraceExp()

// occurrence of trace expression identifier with possibly generic arguments
data class TraceExpVar(val id: TraceExpId, val genericVars: List<VarId>): TraceExp()

data class EventTypeTraceExp(val id: Id, val eventTerms: List<EventTerm>): TraceExp() {
    data class Id(val name: String): AbstractId(name)
}