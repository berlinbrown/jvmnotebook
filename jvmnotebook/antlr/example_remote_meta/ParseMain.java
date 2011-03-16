
import java.io.*;
import org.antlr.*;
import org.antlr.runtime.*;

public class ParseMain {
	public static void main(String [] args)  throws Exception { 
 	        RemoteDefLexer lex = new RemoteDefLexer(
				new ANTLRFileStream(args[0]));
 	        CommonTokenStream tokens = new CommonTokenStream(lex);
			RemoteDefParser g = new RemoteDefParser(tokens);

			System.out.println("Running Parse");
			try {
			    g.root_meta_declarations();
			} catch(Exception e) {
			    e.printStackTrace();
			}
			System.out.println("Done");
	}
}
