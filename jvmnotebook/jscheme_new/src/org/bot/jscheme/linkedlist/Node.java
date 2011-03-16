// File: Node.java
// Date: 7/4/2008
package org.bot.jscheme.linkedlist;

/**
 * A Node has two fields, data and next (or car and cdr).
 * This forms the basis of a simple, singly-linked list structure 
 * whose contents can be manipulated with cons, car, and cdr. 
 * 
 * @author Berlin Brown
 */
public class Node {

	/**
	 * The first element of the pair.
	 */
	public Object data;

	/**
	 *  The other element of the pair.
	 */
	public Node next;

	/** Build a pair from two components. * */
	public Node(Object data, Node next) {
		this.data = data;
		this.next = next;
	}

	/**
	 * Return a String representation of the pair.
	 */
	public String toString() {
		return "" + data;
	}
	public String stringifyNode() {
		return stringifyNode(new StringBuffer());
	}
	public String stringifyNode(StringBuffer buf) {
		buf.append('(');
		buf.append(data);
		Object tail = this.getNext();
		while (tail instanceof Node) {
			buf.append(' ');
			buf.append(((Node) tail).getData());
			tail = ((Node) tail).getNext();
		}
		if (tail != null) {
			buf.append(" . ");
			((Node) tail).stringifyNode(buf);
		}
		buf.append(')');
		return buf.toString();
	}

	public Node getNext() {
		return next;
	}
	public void setNext(Node next) {
		this.next = next;
	}
	public Object getData() {
		return data;
	}
	public void setData(Object data) {
		this.data = data;
	}
}
