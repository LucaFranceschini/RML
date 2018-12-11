package rml

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.RecognitionException
import prolog.PrologCompiler
import rml.ast.toProlog
import rml.parser.buildSpecificationAst
import rml.parser.rmlLexer
import rml.parser.rmlParser
import java.io.IOException

fun main(args: Array<String>) {
    try {
        val input = CharStreams.fromStream(System.`in`)
        val lexer = rmlLexer(input)
        val tokenStream = CommonTokenStream(lexer)
        val parser = rmlParser(tokenStream)
        val parseTree = parser.spec()
        val rmlAst = buildSpecificationAst(parseTree)
        val prologAst = toProlog(rmlAst, "test_trace_expression")
        val writer = System.out.bufferedWriter()
        PrologCompiler(writer).compile(prologAst)
        writer.close()
    } catch (e: IOException) {
        System.err.println(e.message)
    } catch (e: RecognitionException) {
        System.err.println(e.message)
    }
}
