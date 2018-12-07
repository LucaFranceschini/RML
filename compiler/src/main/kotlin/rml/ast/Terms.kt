package rml.ast

// event are event types arguments
sealed class EventTerm
data class VarEventTerm(val id: VarId): EventTerm()
data class IntEventTerm(val number: Int): EventTerm()
data class StringEventTerm(val string: String): EventTerm()