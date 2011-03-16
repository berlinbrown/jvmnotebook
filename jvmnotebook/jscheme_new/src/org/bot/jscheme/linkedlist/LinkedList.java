// File: LinkedList.java
// Date: 7/4/2008
package org.bot.jscheme.linkedlist;

/**
 * A Node has two fields, data and next (or car and cdr).
 * This forms the basis of a simple, singly-linked list structure 
 * whose contents can be manipulated with cons, car, and cdr. 
 * 
 * @author Berlin Brown
 */
public class LinkedList {

	/**
	 * Head element in linked list.
	 */
	public Node head;

	private LinkedList() { }
	
	public LinkedList(Node head) {
		this.head = head;
	}
	public LinkedList(String data) {
		this.head = new Node(data, null);
	}
	
	public Node getHead() {
		return this.head;
	}
	/**
	 * Add a new node and replace the "root" node with the new one.
	 * 
	 * @param node
	 * @return
	 */
	public LinkedList insertHead(final Node node) {
		node.setNext(head);
		head = node;		
		return this;
	}
	public void insertTail(final Node node) {
		if (head == null) {
			head = node;
		} else {
			Node p, q;
			// Traverse to the end of the list
			for (p = head; (q = p.getNext()) != null; p = q) {
				;
			}		        
			p.setNext(node);
		}
	}
	public String traverse() {
		// Append the contents to a string
		StringBuffer buf = new StringBuffer();
		buf.append("(");	
		buf.append(this.getHead());
        for (Node node = this.getHead().getNext(); node != null; node = node.getNext()) {
        	buf.append(' ');
        	buf.append(node.getData());        	
        }        
        buf.append(")");
        return buf.toString();
	}
	
	/**
	 * Perform an arithmetic operation on the List.
	 * @param operation
	 * @return
	 */
	public double numCompute(final char operation) {
		double result = 0.0;
		if (operation == '+' || operation == '-') {
			result = 0.0;
		} else if (operation == '*' || operation == '/') {
			result = 1.0;
		} else {
			throw new RuntimeException("Invalid Operation: try +, -, *, /");
		}
        for (Node node = this.getHead(); node != null; node = node.getNext()) {
        	Double d = new Double((String) node.getData());
        	double x = d.doubleValue();
        	switch (operation) {			
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
				throw new RuntimeException("Invalid Operation: try +, -, *, /");				
			}
        }               
        return result;
	}
	
	public String stringify() {
		return this.getHead().stringifyNode();
	}
	public String toString() {		
		//return this.stringify();
		return this.traverse();
	}
	
	private int passed = 0;
	public Object fail(final String msg, final String a, final String b) {
		testStatus();
		throw new RuntimeException("FAIL: ! Test Failed [" + msg + "] a => " + a + " b => " + b);
	}
	public void testStatus() {
		System.out.println("#Tests passed => " + passed);
	}
	public void assertEquals(final String a, final String b) {
		if ((a == b) && a == null) {
			// TODO: ok, I had a "what is pi" moment with this one
			passed++;
			return;
		} else if ((a == null) || b == null) {
			fail("null found", a, b);
		} else if (a.equals(b)) {
			passed++;
			return;
		} else {
			fail("not equal", a, b);
		}
	}
	
	public static void main(String [] args) {
		System.out.println("Running linked list test");
		
		final LinkedList list = new LinkedList("+");
		list.assertEquals("" + list, "(+)");
		
		// Start off backwards; insert using insertHead.
		list.insertHead(new Node("1", null));		
		list.insertHead(new Node("2", null));
		list.insertHead(new Node("3", null));
		list.assertEquals("" + list, "(3 2 1 +)");
		list.testStatus();
		list.assertEquals(
				"(3 2 1 +)",
				list.getHead().stringifyNode());
		
		final Node node = new Node("+", new Node("1", null));
		list.assertEquals(
				"(+ 1)",
				node.stringifyNode());
		
		final Node node2 = new Node("+", new Node("1", new Node("2", null)));
		list.assertEquals(
				"(+ 1 2)",
				node2.stringifyNode());
		
		final Node node3 = new Node("+", new Node("1", new Node("2", null)));
		final LinkedList list2 = new LinkedList(node3);
		list.assertEquals(
				"(+ 1 2)",
				list2.getHead().stringifyNode());
		list.assertEquals(
				"(+ 1 2)", "" + list2);				
						
		final LinkedList list3 = new LinkedList("+");
		list3.assertEquals("" + list3, "(+)");
		list3.insertTail(new Node("1", null));		
		list3.insertTail(new Node("2", null));
		list3.insertTail(new Node("3", null));
		list.assertEquals("" + list3, "(+ 1 2 3)");		
		list.testStatus();
				
		final LinkedList list4 = new LinkedList("1");		
		list4.insertTail(new Node("2", null));		
		list4.insertTail(new Node("3", null));
		list4.insertTail(new Node("4", null));
		list.assertEquals("" + list4, "(1 2 3 4)");
		list.assertEquals("" + list4.numCompute('+'), "10.0");
		list.assertEquals("" + list4.numCompute('*'), "24.0");
	}
}
