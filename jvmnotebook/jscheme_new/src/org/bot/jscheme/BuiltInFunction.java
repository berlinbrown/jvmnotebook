// File: BuiltInFunction.java
// Date: 7/4/2008
package org.bot.jscheme;

/** 
 * 
 * Original Comments:
 * A primitive is a procedure that is defined as part of the Scheme report,
 * and is implemented in Java code. 
 * 
 * @author Berlin Brown (refactoring modifications)
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html  
 */
public class BuiltInFunction
	extends Procedure implements FunctionConstants {

	/**
	 * Minimum number of args for the function/procedur.
	 */
	private int minArgs;

	private int maxArgs;

	private int idNumber;
	
	public static Double ZERO = new Double(0.0);
	public static Double ONE = new Double(1.0);

	private BuiltInFunction() {
		
	}	
	public BuiltInFunction(int id, int minArgs, int maxArgs) {
		this.idNumber = id;
		this.minArgs = minArgs;
		this.maxArgs = maxArgs;
	}
	
	public static Double num(double x) { 
		return (x == 0.0) ? ZERO : (x == 1.0) ? ONE : new Double(x); 
	}
	public static double num(Object x) { 
		if (x instanceof Number) {
			return ((Number)x).doubleValue();
		} else {
			return num(SchemeUtil.error("expected a number, got: " + x));
		} // End of the if - else
	}
	
	public static Object numCompute(Object args, char op, double result) {		
		if (args == null) {
			switch (op) {
				case '-':
					return num(0 - result);
				case '/':
					return num(1 / result);
				default:
					return num(result);
			}
		} else {
			// Perform the operation against the linked list
			while (args instanceof Pair) {
				double x = num(SchemeUtil.first(args));
				// Traverse
				args = SchemeUtil.rest(args);
				switch (op) {
					case 'X':
						if (x > result)
							result = x;
						break;
					case 'N':
						if (x < result)
							result = x;
						break;
					case '+':
						result += x;
						break;
					case '-':
						result -= x;
						break;
					case '*':
						result *= x;
						break;
					case '/':
						result /= x;
						break;
					default:
						SchemeUtil.error("Internal Error: unrecognized op: " + op);
						break;
					}
			} // end of while
			return num(result);
		}
	}
	
	/** 
	 * Apply a primitive to a list of arguments.
	 */
	public Object apply(Scheme interp, Object args) {
		// First make sure there are the right number of arguments. 
		int nArgs = SchemeUtil.length(args);
		if (nArgs < minArgs) {
			return SchemeUtil.error("too few args, " + nArgs + ", for " + this.getName() + ": "
					+ args);
		} else if (nArgs > maxArgs) {
			return SchemeUtil.error("too many args, " + nArgs + ", for " + this.getName()
					+ ": " + args);
		} // End of the If

		Object x = SchemeUtil.first(args);
		Object y = SchemeUtil.second(args);

		switch (idNumber) {
			case PLUS:
				return numCompute(args, '+', 0.0);
			case MINUS:
				return numCompute(SchemeUtil.rest(args), '-', num(x));
			case TIMES:
				return numCompute(args, '*', 1.0);
			case DIVIDE:
				return numCompute(SchemeUtil.rest(args), '/', num(x));			
			case THIRD:
				return SchemeUtil.third(x);
			case CONS:
				return SchemeUtil.cons(x, y);
			case CAR:
				return SchemeUtil.first(x);
			case CDR:
				return SchemeUtil.rest(x);
			case CXR:
				for (int i = this.getName().length() - 2; i >= 1; i--) {
					x = (this.getName().charAt(i) == 'a') ? SchemeUtil.first(x) : SchemeUtil.rest(x);
				}
				return x;
			default:
				return SchemeUtil.error("internal error: unknown primitive: " + this
						+ " applied to " + args);
		}
	} // End of the Apply
	
	/**
	 * Define the procedures for the lisp environment.
	 */
	public static Environment installBuiltInFunctions(Environment env)  {
	    int n = Integer.MAX_VALUE;
	    env
	    
	     .defineBuiltInProc("cons",     CONS,      2)	     
	    
	     .defineBuiltInProc("*",       	TIMES,     0, n)	     
	     .defineBuiltInProc("+",       	PLUS,      0, n)
	     .defineBuiltInProc("-",       	MINUS,     1, n)
	     .defineBuiltInProc("/",       	DIVIDE,    1, n)
	    
	     .defineBuiltInProc("caaaar",         CXR,       1)
	     .defineBuiltInProc("caaadr",         CXR,       1)
	     .defineBuiltInProc("caaar",          CXR,       1)
	     .defineBuiltInProc("caadar",         CXR,       1)
	     .defineBuiltInProc("caaddr",         CXR,       1)
	     .defineBuiltInProc("caadr",          CXR,       1)
	     .defineBuiltInProc("caar",           CXR,       1)	     
	     .defineBuiltInProc("caddr",     	THIRD,       1)
	     .defineBuiltInProc("cadr",  	    SECOND,      1)	   	     
	     .defineBuiltInProc("cdaaar",         CXR,       1)
	     .defineBuiltInProc("cdadr",          CXR,       1)
	     .defineBuiltInProc("cdar",           CXR,       1)
	     .defineBuiltInProc("cddaar",         CXR,       1)
	     .defineBuiltInProc("cddr",           CXR,       1)
	     
	     .defineBuiltInProc("car",     	      CAR,       1)
	     .defineBuiltInProc("cdr",     	      CDR,       1)	    
	    	     	
	     .defineBuiltInProc("*",       	TIMES,     0, n)	     
	     .defineBuiltInProc("+",       	PLUS,      0, n)
	     .defineBuiltInProc("-",       	MINUS,     1, n)
	     .defineBuiltInProc("/",       	DIVIDE,    1, n);
	    return env;
	}	
}
