package compiler.prolog

import compiler.prolog.ast.*
import java.io.BufferedWriter

class PrologCompiler(private val writer: BufferedWriter) {
    private fun compile(term: Term): Unit = when (term) {
        is IntTerm -> writer.write(term.number.toString())
        is FloatTerm -> writer.write(term.number.toString())
        is VariableTerm -> writer.write(term.identifier.string)
        is CompoundTerm -> compile(term)
        is DictionaryTerm -> {
            compile(term.tag)
            intersperse(term.pairs, PrologCompiler::compile, "{", "}")
        }
        EmptyList -> writer.write("[]")
    }

    private fun compile(pair: DictionaryTerm.KeyValuePair) {
        compile(pair.key)
        writer.write(":")
        compile(pair.value)
    }

    // this function does some effort to pretty print terms
    private fun compile(term: CompoundTerm) {
        // print alphabetical atoms in the simplest way
        // make sure they start with a lowercase not to generate variables
        if (term.args.isEmpty() && term.functor.matches(Regex("[a-z]\\w*"))) {
            writer.write(term.functor)
        }
        // lists are handled in a special way
        else if (term.functor == "[|]" && term.args.size == 2) {
            compileList(term)
        }
        // if it's an atom just print it quoted
        else if (term.arity == 0) {
            writer.write("'${term.functor}'")
        }
        // if functor is binary and not a word, print as infix
        else if (term.args.size == 2 && !term.functor.first().isLetter()) {
            // use parentheses to avoid precedence problems
            intersperse(term.args, prefix = "(", suffix = ")", separator = term.functor)
        } else {
            // otherwise use function notation
            writer.write(term.functor)
            intersperse(term.args, "(", ")")
        }
    }

    private fun compileList(term: CompoundTerm) {
        assert(term.args.size == 2) { "number of list constructor arguments should be 2" }
        intersperse(unfoldList(term), prefix = "[", suffix = "]")
    }

    private fun unfoldList(term: Term): List<Term> {
        if (term == EmptyList)
            return emptyList()

        if (term is CompoundTerm) {
            assert(term.args.size == 2) { "number of list constructor arguments should be 2" }
            return listOf(term.args[0]) + unfoldList(term.args[1])
        }

        error("unexpected constructor, list expected")
    }

    private fun compile(clause: Clause) {
        compile(clause.head)

        if (clause.body.isNotEmpty()) {
            writer.write(" :- ")
            intersperse(clause.body)
        }

        writer.write(".\n")
    }

    private fun compile(directive: Directive): Unit =
        intersperse(directive.body, prefix = ":- ", suffix = ".\n")

    // util function to conveniently compile many things with a separator
    private fun <T> intersperse(terms: List<T>,
                                compileFunction: PrologCompiler.(T) -> Unit, // a method of this class
                                prefix: String = "",
                                suffix: String = "",
                                separator: String = ", ") {
        writer.write(prefix)

        if (terms.isNotEmpty()) {
            this.compileFunction(terms.first())

            for (term in terms.drop(1)) {
                writer.write(separator)
                this.compileFunction(term)
            }
        }

        writer.write(suffix)
    }

    // simpler version of the function above for terms
    private fun intersperse(terms: List<Term>,
                            prefix: String = "",
                            suffix: String = "",
                            separator: String = ", ") =
            intersperse(terms, PrologCompiler::compile, prefix, suffix, separator)

    fun compile(program: Program) {
        program.directives.map(::compile)
        program.clauses.map(::compile)
    }
}