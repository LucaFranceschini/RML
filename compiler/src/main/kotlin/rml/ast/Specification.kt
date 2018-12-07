package rml.ast

// top-level trace expression specification
data class Specification(val declarations: List<Declaration>) {
    init {
        if (declarations.isEmpty())
            throw IllegalArgumentException("specification must contain at least one trace expression")
    }
}

// possibly generic trace expression as an equation
data class Declaration(val id: TraceExpId, val vars: List<VarId>, val traceExp: TraceExp)