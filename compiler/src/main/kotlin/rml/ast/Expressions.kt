package rml.ast

// expressions are event types arguments
sealed class Exp
data class VarExp(val id: VarId): Exp()
data class IntExp(val number: Int): Exp()
data class StringExp(val string: String): Exp()