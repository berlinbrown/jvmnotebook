package org.bot.jscheme.test;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;

import org.bot.jscheme.InputReader;
import org.bot.jscheme.Pair;

public class TestInputReader extends TestCase {

	public static final String test1 = "(+ 1 2 3)  ";

	protected void setUp() throws Exception {
	}

	protected void tearDown() throws Exception {
	}

	public void testInputReaderStream() {
		ByteArrayInputStream stream = new ByteArrayInputStream(test1.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		assertNotNull(o);
	}
	public void testInputReaderPair() {
		ByteArrayInputStream stream = new ByteArrayInputStream(test1.getBytes());
		InputReader reader = new InputReader(stream);
		Object o = reader.read();
		Pair p = (Pair) o;
		assertEquals("(+ 1.0 2.0 3.0)", "" + p);
	}
}
