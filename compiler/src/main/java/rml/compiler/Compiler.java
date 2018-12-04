package rml.compiler;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;

public class Compiler {
    public static void main(String[] args) {
        try {
            var input = CharStreams.fromStream(System.in);
            var lexer = new rmlLexer(input);
            var tokenStream = new CommonTokenStream(lexer);
            var parser = new rmlParser(tokenStream);
            System.out.println(parser.spec().getPayload().getText());
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
