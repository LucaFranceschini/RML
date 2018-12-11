package prolog.ast

sealed class PrologTerm

data class IntTerm(val number: Int): PrologTerm()

data class VarTerm(val name: String): PrologTerm()

data class FunctionTerm(val functionSymbol: FunctionSymbol, val args: List<PrologTerm>): PrologTerm() {
    val arity: Int = args.size

    data class FunctionSymbol(val name: String) {
        init {
            require(name.isNotBlank()) { "blank functor identifier" }
        }
    }

    // implicitly build the predicate
    constructor(name: String, args: List<PrologTerm>): this(FunctionSymbol(name), args)

    // also use varargs
    constructor(name: String, vararg args: PrologTerm): this(FunctionSymbol(name), args.toList())

    companion object {
        fun constant(name: String): FunctionTerm = FunctionTerm(name, emptyList())
    }
}

data class ListTerm(val headTerms: List<PrologTerm>, val tail: PrologTerm? = null): PrologTerm() {
    init {
        require(headTerms.isNotEmpty() || tail == null) { "can't have tail term without other terms first" }
    }
}