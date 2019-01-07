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
import com.xenomachina.argparser.ArgParser
import com.xenomachina.argparser.DefaultHelpFormatter
import com.xenomachina.argparser.default
import com.xenomachina.argparser.mainBody
import java.io.File
import java.io.InputStream
import java.io.OutputStream


class Args(parser: ArgParser) {
    val input by parser.storing("input file").default<String?>(null)
    val output by parser.storing("output file").default<String?>(null)
}

// see documentation https://github.com/xenomachina/kotlin-argparser
fun main(args: Array<String>) = mainBody {
    // define help message
    val help = DefaultHelpFormatter("By default the program reads from standard input and writes to standard output")

    ArgParser(args, helpFormatter = help).parseInto(::Args).run {
        val inputStream = if (input != null) File(input).inputStream() else System.`in`
        val outputStream = if (output != null) File(output).outputStream() else System.out
        compile(inputStream, outputStream)
    }
}

fun compile(inputStream: InputStream, outputStream: OutputStream) {
    try {
        val input = CharStreams.fromStream(inputStream)
        val lexer = rmlLexer(input)
        val tokenStream = CommonTokenStream(lexer)
        val parser = rmlParser(tokenStream)
        val parseTree = parser.spec()
        val rmlAst = buildSpecificationAst(parseTree)
        val prologAst = toProlog(rmlAst)
        val writer = outputStream.bufferedWriter()
        PrologCompiler(writer).compile(prologAst)
        writer.close()
    } catch (e: IOException) {
        System.err.println(e.message)
    } catch (e: RecognitionException) {
        System.err.println(e.message)
    }
}
