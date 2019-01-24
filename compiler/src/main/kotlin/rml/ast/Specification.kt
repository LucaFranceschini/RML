package rml.ast

// top-level trace expression specification
data class Specification(val evtypeDecls: List<EvtypeDecl>, val traceExpDecls: List<TraceExpDecl>, val mainTraceExp: TraceExpId) {
    init {
        require(traceExpDecls.isNotEmpty()) { "specification must contain at least one trace expression" }
        require(traceExpDecls.any { it.id == mainTraceExp }) { "main trace expression not declared" }
        require(traceExpDecls.map { it.id }.distinct() == traceExpDecls) { "trace expression declared multiple times" }
    }
}

// possibly generic trace expression as an equation
data class TraceExpDecl(val id: TraceExpId, val vars: List<VarId>, val traceExp: TraceExp)