package org.bot.jscheme.linkedlist;

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

}
