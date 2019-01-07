package prolog

import prolog.ast.*
import java.io.BufferedWriter

class PrologCompiler(private val writer: BufferedWriter) {
    private fun compile(term: PrologTerm): Unit = when (term) {
        is IntTerm -> writer.write(term.number)
        is VarTerm -> writer.write(term.name)
        is FunctionTerm -> compile(term)
        is ConstantTerm -> writer.write("'${term.string}'")
        is ListTerm -> {
            writer.write("[")

            // note that ListTerm ensures tail term is only allowed in presence of head terms before them

            // head terms
            if (term.headTerms.isNotEmpty()) {
                compile(term.headTerms[0])
                for (headTerm in term.headTerms.drop(1)) {
                    writer.write(", ")
                    compile(headTerm)
                }
            }

            // tail
            if (term.tail != null) {
                writer.write("|")
                compile(term.tail)
            }

            writer.write("]")
        }
    }

    private fun compile(term: FunctionTerm) {
        val functor = term.functionSymbol.name

        // constants
        if (term.arity == 0)
            writer.write(functor)
        // use infix notation for binary symbolic functors
        else if (term.arity == 2 && functor.matches(Regex("[^a-zA-Z0-9_]*"))) {
            // avoid ambiguity using parentheses
            writer.write("(")
            compile(term.args[0])
            writer.write(functor)
            compile(term.args[1])
            writer.write(")")
        } else { // prefix notation otherwise
            writer.write(functor)
            writer.write("(")
            compile(term.args.first())
            for (arg in term.args.drop(1)) {
                writer.write(", ")
                compile(arg)
            }
            writer.write(")")
        }
    }

    private fun compile(atom: Atom) {
        val symbol = atom.symbol.name

        // print unification as infix
        if (symbol == "=") {
            assert(atom.args.size == 2) { "unification must have 2 arguments" }
            compile(atom.args.first())
            writer.write(symbol)
            compile(atom.args[1])
        } else { // prefix syntax otherwise
            writer.write(symbol)

            if (atom.arity > 0) {
                writer.write("(")
                compile(atom.args.first())
                for (arg in atom.args.drop(1)) {
                    writer.write(", ")
                    compile(arg)
                }
                writer.write(")")
            }
        }
    }

    private fun compile(clause: Clause): Unit {
        compile(clause.head)

        if (clause.body.isNotEmpty()) {
            writer.write(" :- ")
            compile(clause.body.first())
            for (atom in clause.body.drop(1)) {
                writer.write(", ")
                compile(atom)
            }
        }

        writer.write(".")
    }

    fun compile(program: LogicProgram): Unit {
        for (clause in program.clauses) {
            compile(clause)
            writer.newLine()
        }
    }
}