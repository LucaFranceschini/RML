package compiler.prolog.ast

import java.nio.charset.StandardCharsets

sealed class Term

data class IntTerm(val number: Int): Term()
data class FloatTerm(val number: Double): Term()

data class VariableTerm(val identifier: Identifier): Term() {
    // allow construction from strings
    constructor(identifier: String): this(Identifier(identifier))

    companion object {
        val ignored = VariableTerm("_")
    }
}

// f(term1, ..., termN) (including constants, strings...)
data class CompoundTerm(val functor: String, val args: List<Term>): Term() {
    val arity: Int = args.size

    // allow varargs
    constructor(functor: String, vararg args: Term): this(functor, args.toList())

    companion object {
        // utility method to generate lists
        fun list(vararg terms: Term): Term =
                if (terms.isEmpty()) EmptyList
                else CompoundTerm("[|]", terms.first(), list(*terms.drop(1).toTypedArray()))
    }
}

// compound terms with arity 0 are often called atoms
fun atom(string: String) = CompoundTerm(string)

// tag{key1:value1, ..., keyN:valueN}
// they are first-class citizens in SWI-Prolog
data class DictionaryTerm(val tag: Term, val pairs: List<KeyValuePair>): Term() {
    data class KeyValuePair(val key: Term, val value: Term)
}

// empty lists are special from SWI-Prolog v7
object EmptyList: Term()

data class Identifier(val string: String) {
    init {
        require(string.isNotBlank()) { "blank identifier not allowed" }
        require(StandardCharsets.US_ASCII.newEncoder().canEncode(string)) { "ASCII string expected" }
    }
}