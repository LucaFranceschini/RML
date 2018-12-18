package rml.ast

// top-level trace expression specification
data class Specification(val declarations: List<TraceExpDecl>, val mainTraceExp: TraceExpId) {
    init {
        require(declarations.isNotEmpty()) { "specification must contain at least one trace expression" }
        require(declarations.any { it.id == mainTraceExp }) { "main trace expression not declared" }
        require(declarations.distinct() == declarations) { "trace expression declared multiple times" }
    }
}

// possibly generic trace expression as an equation
data class TraceExpDecl(val id: TraceExpId, val vars: List<VarId>, val traceExp: TraceExp)