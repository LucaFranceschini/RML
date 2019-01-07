package prolog.ast

data class Atom(val symbol: PredicateSymbol, val args: List<PrologTerm>) {
    val arity = args.size

    data class PredicateSymbol(val name: String) {
        init {
            if (name.isBlank())
                throw IllegalArgumentException("empty predicate symbol")
        }
    }

    constructor(symbol: String, vararg args: PrologTerm): this(PredicateSymbol(symbol), args.toList())
}

data class Clause(val head: Atom, val body: List<Atom>) {
    constructor(head: Atom, vararg bodyAtoms: Atom): this(head, bodyAtoms.toList())
}

data class LogicProgram(val clauses: List<Clause>) {
    constructor(vararg clauses: Clause): this(clauses.toList())
}