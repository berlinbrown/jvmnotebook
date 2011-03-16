// File: SchemeUtil.java
// Date: 7/4/2008
package org.bot.jscheme;

/**
 * Collection static scheme interpreter utilities. 
 */
public abstract class SchemeUtil {

	public static Object error(String message) {
		System.err.println("ERROR: " + message);
		throw new RuntimeException(message);  
	}
	
	/**
	 * Like Common Lisp first; car of a Pair, or null for anything else.
	 */
	public static Object first(Object x) {
		return (x instanceof Pair) ? ((Pair) x).getFirst() : null;
	}
	public static Object third(Object x) {
		return first(rest(rest(x)));
	}

	/**
	 * Like Common Lisp rest; car of a Pair, or null for anything else.
	 * "A cons cell is composed of two pointers; the car operation 
	 * extracts the first pointer, and the cdr operation extracts the second."
	 * 
	 * "Thus, the expression (car (cons x y)) evaluates to x, 
	 * and (cdr (cons x y)) evaluates to y."
	 */
	public static Object rest(Object x) {
		return (x instanceof Pair) ? ((Pair) x).getRest() : null;
	}

	/**
	 * Like Common Lisp second.
	 */
	public static Object second(Object x) {
		return first(rest(x));
	}
	
	/** 
	 * The length of a list, or zero for a non-list. 
	 */
	public static int length(Object x) {
		int len = 0;
	    while (x instanceof Pair) {
	      len++;
	      x = ((Pair)x).getRest();
	    }
	    return len;
	}

	/**
	 * Creates a two element list.
	 */
	public static Pair list(Object a, Object b) {
		return new Pair(a, new Pair(b, null));
	}

	/**
	 * Creates a one element list.
	 */
	public static Pair list(Object a) {
		return new Pair(a, null);
	}

	/**
	 * cons(x, y) is the same as new Pair(x, y).
	 * 
	 * Cons presents and interesting function that is fundamental to lisp.
	 * Here are some examples of cons usage (tested in common lisp).
	 * <code>
	 * (cons 1 2):
	 * Pair pair = SchemeUtil.cons("1", "2");
	 * assertEquals("" + pair, "(1 . 2)");
	 * 
	 * (cons 1 nil):
	 * Pair pair = SchemeUtil.cons("1", null);
	 * assertEquals("" + pair, "(1)");
	 * 
	 * (cons 1 (cons 2 nil)):
	 * 
	 * Pair pair = SchemeUtil.cons("1", SchemeUtil.cons("2", null));
	 * assertEquals("" + pair, "(1 2)");
	 *  
	 * </code>
	 */
	public static Pair cons(Object a, Object b) {
		return new Pair(a, b);
	}
	
	private static void stringifyObjectArray(final Object [] x, final StringBuffer buf) {
    	Object[] v = (Object[]) x;
    	buf.append("#(");
    	for (int i = 0; i < v.length; i++) {
      			stringify(v[i], buf);
      			if (i != v.length - 1) { 
      				buf.append(' ');
      			}
    	} // End of For
    	buf.append(')');
	}
	
	/**
	 * Convert a Scheme object to its printed representation, as a java String
	 * (not a Scheme string).
	 */
	public static void stringify(Object x, final StringBuffer buf) { 
	    if (x == null) {
	      buf.append("()");
	    } else if (x instanceof Double) {
	      double d = ((Double)x).doubleValue();
	      buf.append(d);
	    } else if (x instanceof Character) {	      
	      buf.append(x);
	    } else if (x instanceof Pair) {
	      ((Pair) x).stringifyPair(buf);
	    } else if (x instanceof char[]) {	    	
	    	char[] chars = (char[]) x;	        
	        for (int i = 0; i < chars.length; i++) {	  	
	        	buf.append(chars[i]);
	        } // End of For
	    } else if (x instanceof Object []) {	    	
	    	stringifyObjectArray((Object []) x, buf);
	    } else if (x == Boolean.TRUE) {
	      buf.append("#t");
	    } else if (x == Boolean.FALSE) {
	      buf.append("#f");
	    } else {
	      buf.append(x);
	    }
	} // End of Method
	
	/** 
	 * Convert x to a Java String giving its external representation. 
	 */
	public static String stringify(Object x) { 
	    StringBuffer buf = new StringBuffer();
	    stringify(x, buf);
	    return buf.toString();
	}
	
} // End of the Class
