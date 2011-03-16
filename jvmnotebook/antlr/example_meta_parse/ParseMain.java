
import java.io.*;
import org.antlr.*;
import org.antlr.runtime.*;

import java.util.Set;
import java.util.Stack;
import java.util.Map;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;

public class ParseMain {
	public static void main(String [] args)  throws Exception {
 	        RemoteDefLexer lex = new RemoteDefLexer(
				new ANTLRFileStream(args[0]));
 	        CommonTokenStream tokens = new CommonTokenStream(lex);
			RemoteDefParser g = new RemoteDefParser(tokens);

			System.out.println("Running Parse");
			try {
			    g.root_meta_declarations();

			    final Map map = g.getRootNamespaceAttributes();			    
			    final Iterator _it = map.entrySet().iterator();
			    
			    System.out.println("INFO: ROOT ATTRIBUTES: sz=" + map.size());
			    while(_it.hasNext()) {
					Object t = _it.next();
					System.out.println(t);
				}
			    
			    final Stack operationStack = g.getRootOperations();
			    final Iterator _it_stack = operationStack.iterator();
			    
			    System.out.println("INFO: ROOT Operations: sz=" + operationStack.size());
			    while(_it_stack.hasNext()) {
					Object t = _it_stack.next();
					System.out.println(t);
				}
			    
			} catch(Exception e) {
			    e.printStackTrace();
			}
			System.out.println("Done");
	}
}
