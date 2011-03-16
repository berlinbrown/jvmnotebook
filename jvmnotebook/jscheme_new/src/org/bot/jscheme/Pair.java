// File: Pair.java
// Date: 7/4/2008
package org.bot.jscheme;

/**
 * A Pair has two fields, first and rest (or car and cdr).
 * This forms the basis of a simple, singly-linked list structure 
 * whose contents can be manipulated with cons, car, and cdr. 
 * 
 * "One mathematical consideration that influenced LISP was to 
 * express programs as applicative expressions built up 
 * from variables and constants using functions" -- John McCarthy
 * http://www-formal.stanford.edu/jmc/history/lisp/node3.html
 */
public class Pair {

	/**
	 * The first element of the pair.
	 */
	private Object first;

	/**
	 *  The other element of the pair.
	 */
	private Object rest;

	/** Build a pair from two components. * */
	public Pair(Object first, Object rest) {
		this.first = first;
		this.rest = rest;
	}

	/**
	 * Return a String representation of the pair.
	 */
	public String toString() {
		return SchemeUtil.stringify(this);
	}

	/**
	 * Build up a String representation of the Pair in a StringBuffer.
	 */
	void stringifyPair(StringBuffer buf) {
		buf.append('(');
		SchemeUtil.stringify(first, buf);
		Object tail = rest;
		while (tail instanceof Pair) {
			buf.append(' ');
			SchemeUtil.stringify(((Pair) tail).first, buf);
			tail = ((Pair) tail).rest;
		}
		if (tail != null) {
			buf.append(" . ");
			SchemeUtil.stringify(tail, buf);
		}
		buf.append(')');
	}

	/**
	 * @return the first
	 */
	public Object getFirst() {
		return first;
	}
	
	/**
	 * @return the rest
	 */
	public Object getRest() {
		return rest;
	}
}
