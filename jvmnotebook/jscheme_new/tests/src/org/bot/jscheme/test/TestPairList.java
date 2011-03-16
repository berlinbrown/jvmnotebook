package org.bot.jscheme.test;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;

import org.bot.jscheme.BuiltInFunction;
import org.bot.jscheme.Environment;
import org.bot.jscheme.InputReader;
import org.bot.jscheme.Pair;
import org.bot.jscheme.Scheme;
import org.bot.jscheme.SchemeUtil;

/**
 * Basic test of the pair functionality. 
 */
public class TestPairList extends TestCase {
	
	public static final String test1 = "(cons 1 2)";
	public static final String test2 = "(cons 1 (cons 2 ()))";
	
	public void testBasicBuildPair() {
		// Construct a linked list/pair of
		// a function '+', 1 and 2
		// (+ 1 2)
		Pair pair = new Pair("+", new Pair("1", new Pair("2", null)));
		assertEquals("" + pair, "(+ 1 2)");
	}	
	public void testPair1() {
		// Construct a linked list/pair of
		// a function '+', 1 and 2
		// (+ 1 2)
		Pair pair = new Pair("+", new Pair("1", new Pair("2", ":test")));		
		assertEquals("" + pair, "(+ 1 2 . :test)");
	}	
	/**
	 * (cons 1 2).	
	 */
	public void testCons() {
		Pair pair = SchemeUtil.cons("1", "2");
		assertEquals("" + pair, "(1 . 2)");
	}
	/**
	 * (cons 1 nil).
	 */
	public void testConsOne() {
		Pair pair = SchemeUtil.cons("1", null);
		assertEquals("" + pair, "(1)");
	}
	/**
	 * (cons 1 (cons 2 nil)).	 
	 */
	public void testConsTwo() {
		Pair pair = SchemeUtil.cons("1", SchemeUtil.cons("2", null));
		assertEquals("" + pair, "(1 2)");
	}
	
	public void testEvalCons1() {
		Environment globalEnvironment = new Environment();
		BuiltInFunction.installBuiltInFunctions(globalEnvironment);
		ByteArrayInputStream stream = new ByteArrayInputStream(test1.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Scheme scheme = new Scheme();
		Object res = scheme.eval(o, globalEnvironment);
		System.out.println(res);
	}
	public void testEvalCons2() {
		Environment globalEnvironment = new Environment();
		BuiltInFunction.installBuiltInFunctions(globalEnvironment);
		ByteArrayInputStream stream = new ByteArrayInputStream(test2.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Scheme scheme = new Scheme();
		Object res = scheme.eval(o, globalEnvironment);
		assertEquals("" + res, "(1.0 2.0)");
	}
}
