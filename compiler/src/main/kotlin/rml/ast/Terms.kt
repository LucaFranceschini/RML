package rml.ast

// terms are event types arguments
sealed class Term
data class VarTerm(val id: VarId): Term()
data class IntTerm(val number: Int): Term()
data class StringTerm(val string: String): Term()