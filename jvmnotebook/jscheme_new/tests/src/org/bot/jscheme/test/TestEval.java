package org.bot.jscheme.test;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;

import org.bot.jscheme.BuiltInFunction;
import org.bot.jscheme.Environment;
import org.bot.jscheme.InputReader;
import org.bot.jscheme.Scheme;

/**
 * Basic test of the eval functionality.  Test number
 * operations (Plus, Minus, Divide).  Instantiate
 * an environment and invoke the eval method against
 * the input lisp code. 
 */
public class TestEval extends TestCase {

	/**
	 * Example lisp function call to a numeric operation:
	 * Add 1 plus 2 plus 3. 
	 */
	public static final String test1 = "(+ 1 2 3)  ";
	
	/**
	 * Example lisp code snippet, call the production function
	 * against all of the arguments.
	 */
	public static final String test2 = " ( * 2 4 6)  ";
	
	public static final String test3 = " (* 3 (* 2 4 6))  ";
	
	protected void setUp() throws Exception {
	}

	protected void tearDown() throws Exception {
	}
	public void testEval1() {
		Environment globalEnvironment = new Environment();
		BuiltInFunction.installBuiltInFunctions(globalEnvironment);
		ByteArrayInputStream stream = new ByteArrayInputStream(test1.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Scheme scheme = new Scheme();
		Object res = scheme.eval(o, globalEnvironment);
		double d = ((Double) res).doubleValue();
		assertEquals(d, 6.0d, 0.01d);
	}
	public void testEval2() {
		Environment globalEnvironment = new Environment();
		BuiltInFunction.installBuiltInFunctions(globalEnvironment);
		ByteArrayInputStream stream = new ByteArrayInputStream(test2.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Scheme scheme = new Scheme();
		Object res = scheme.eval(o, globalEnvironment);
		double d = ((Double) res).doubleValue();
		assertEquals(d, 48.0d, 0.01d);
	}
	public void testEval3() {
		Environment globalEnvironment = new Environment();
		BuiltInFunction.installBuiltInFunctions(globalEnvironment);
		ByteArrayInputStream stream = new ByteArrayInputStream(test3.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Scheme scheme = new Scheme();
		Object res = scheme.eval(o, globalEnvironment);
		double d = ((Double) res).doubleValue();
		assertEquals(d, 144.0d, 0.01d);
	}
}
