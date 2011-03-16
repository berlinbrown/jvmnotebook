// File: Environment.java
// Date: 7/4/2008
package org.bot.jscheme;

import java.util.HashMap;
import java.util.Map;

/** 
 * Environments allow you to look up the value of a variable, given
 * its name.  Keep a list of variables and values, and a pointer to
 * the parent environment.
 * 
 * This code has changed significantly from Norvig's original code. Instead
 * of using a linked-list/cons-pair structure, we use a hashtable to look up
 * the variable definitions. 
 */
public class Environment {
	
	private Map mapDataVars = new HashMap();
	
	public Environment() {
	}

	/** 
	 * Find the value of a symbol, in this environment or a parent. 
	 */
	public Object lookup(String symbol) {
		Object o = this.mapDataVars.get(symbol);
		if (o != null) {
			return o;
		} else {
			return SchemeUtil.error("Unbound variable: [" + symbol + "]");
		}
	}

	/** Add a new variable,value pair to this environment. */
	public Object define(Object var, Object val) {
		
		this.mapDataVars.put(var, val);			
		if (val instanceof Procedure
				&& ((Procedure) val).getName().equals(Procedure.DEFAULT_NAME)) {
			((Procedure) val).setName(var.toString());
		}
		return var;
	}
	
	public Environment defineBuiltInProc(String name, int id, int minArgs) {
		define(name, new BuiltInFunction(id, minArgs, minArgs));
		return this;
	}

	public Environment defineBuiltInProc(String name, int id, int minArgs, int maxArgs) {
		define(name, new BuiltInFunction(id, minArgs, maxArgs));
		return this;
	}	
}
