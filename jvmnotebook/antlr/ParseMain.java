
import java.io.*;
import org.antlr.*;
import org.antlr.runtime.*;

public class ParseMain {
	public static void main(String [] args)  throws Exception {
 	        AnsiCLexer lex = new AnsiCLexer(
			new ANTLRFileStream(args[0]));
 	        CommonTokenStream tokens = new CommonTokenStream(lex);
		AnsiCParser g = new AnsiCParser(tokens);
        	try {
            		//g.translation_unit();
            		g.function_definition();
        	} catch (RecognitionException e) {
            		e.printStackTrace();
        	}
	}
}
