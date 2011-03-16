// File: Procedure.java
// Date: 7/4/2008
package org.bot.jscheme;

/**
 * Procedure Type, use the EnvironmentdefineBuiltInProc method to 
 * setup a procedure for a particular environment.
 * 
 * @author Berlin Brown (refactoring modifications)  
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html
 */
public abstract class Procedure {

	public static String DEFAULT_NAME = "Anonymous Procedure";
	
	private String name = DEFAULT_NAME;

	public String toString() {
		return "{" + name + "}";
	}
	
	public abstract Object apply(Scheme interpreter, Object args);

	/** 
	 * Coerces a Scheme object to a procedure.
	 */
	public static Procedure proc(Object x) {
		if (x instanceof Procedure) {
			return (Procedure) x;
		} else {
			return proc(SchemeUtil.error("Not a procedure: " + SchemeUtil.stringify(x)));
		} // End of the if - else
	}
	public String getName() {		
		return name;
	}
	public void setName(final String n) {
		this.name = n;
	}
	
}
