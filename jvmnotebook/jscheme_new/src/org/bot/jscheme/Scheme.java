// File: Scheme.java
// Date: 7/4/2008
package org.bot.jscheme;

/** 
 * This class represents a Scheme interpreter.
 * See http://www.norvig.com/jscheme.html for more documentation.
 * 
 * @author Berlin Brown (refactoring modifications)
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com 
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html
 */
public class Scheme {

	private Environment globalEnvironment = new Environment();
	
	public static void main(final String [] args) {
		System.out.println("Running Interpreter");
	}
	public Object eval(Object x) {
		return eval(x, this.globalEnvironment);
	}
	public Object eval(Object x, Environment env) {	
		while (true) {
			if (x instanceof String) {			
				// VARIABLE
				System.out.println("trace: eval - instance of String: " + x);
				// Look up a variable or a procedure (built in function).
				return env.lookup((String) x);				
			} else if (!(x instanceof Pair)) { 
				// CONSTANT
				System.out.println("trace: eval - instance of Pair =>" + x);
				return x;
			} else {
				// Procedure Call
				System.out.println("trace: eval[t1] - instance of procedure call");
				Object fn = SchemeUtil.first(x);
				Object args = SchemeUtil.rest(x);
				System.out.println("trace: eval[t2] - fn => [" + fn + "] args => " + args);
				fn = eval(fn, env);
				// Coerce the object to a procedure (E.g. '+/add' procedure)
				Procedure p = Procedure.proc(fn);				
				return p.apply(this, evalList(args, env));									
			} // End of if - else
		} // End of While
	} // End of Method
	public Pair evalList(Object list, Environment env) {
		if (list == null)
			return null;
		else if (!(list instanceof Pair)) {
			final String msg = "Illegal arg list: " + list;
			System.err.println(msg);
			throw new RuntimeException(msg);			
		} else {
			final Object first = eval(SchemeUtil.first(list), env);
			final Object rest = evalList(SchemeUtil.rest(list), env);
			return SchemeUtil.cons(first, rest);						
		}
	}
}
