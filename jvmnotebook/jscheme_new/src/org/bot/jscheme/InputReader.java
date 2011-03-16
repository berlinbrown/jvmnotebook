// File: InputReader.java
// Date: 7/4/2008
package org.bot.jscheme;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Stack;

/** 
 * Generic input reader class for most scheme functions.  The
 * InputReader also includes the token parsing functionality.
 *  
 * @author Berlin Brown (refactoring modifications)
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com  
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html 
 */
public class InputReader {

	public static final String EOF = "#!EOF";

	public static final char TOK_LEFT_PAREN = '('; 
	public static final char TOK_RIGHT_PAREN = ')'; 
	public static final char TOK_SINGLE_QUOT = '\''; 
	public static final char TOK_SEMICOLON = ';';
	public static final char TOK_DOUBLE_QUOT = '"'; 
	public static final char TOK_COMMA = ',';
	public static final char TOK_BACK_QUOT = '`';
	
	private Reader inputReader;

	private StringBuffer buff = new StringBuffer();

	private Stack charStack = new Stack();
	private Stack tokenStack = new Stack();

	public InputReader(InputStream in) {
		this.inputReader = new InputStreamReader(new BufferedInputStream(in));
	}

	/**
	 * (This code is only included to show you a lighter implementation of this
	 * method)
	 * 
	 * Determines if a character is Java whitespace. This includes Unicode
	 * space characters (SPACE_SEPARATOR, LINE_SEPARATOR, and
	 * PARAGRAPH_SEPARATOR) except the non-breaking spaces
	 * (<code>'\u00A0'</code>, <code>'\u2007'</code>, and <code>'\u202F'</code>);
	 * and these characters: <code>'\u0009'</code>, <code>'\u000A'</code>,
	 * <code>'\u000B'</code>, <code>'\u000C'</code>, <code>'\u000D'</code>,
	 * <code>'\u001C'</code>, <code>'\u001D'</code>, <code>'\u001E'</code>,
	 * and <code>'\u001F'</code>.
	 * <br>
	 * 
	 * Java whitespace = ([Zs] not Nb)|[Zl]|[Zp]|U+0009-U+000D|U+001C-U+001F
	 * 
	 * This implemenation taken from GNU classpath:
	 * 
	 * GNU Classpath is free software; you can redistribute it and/or modify
	 * it under the terms of the GNU General Public License
	 *
	 * @param ch character to test
	 * @return true if ch is Java whitespace, else false
	 * @see #isSpaceChar(char)
	 * @since 1.1	 
	 */
	private static final int TYPE_MASK = 0x1F;
	private static final int NO_BREAK_MASK = 0x20;
	private static final byte SPACE_SEPARATOR = 12;
	private static final byte LINE_SEPARATOR = 13;
	private static final byte PARAGRAPH_SEPARATOR = 14;
	public static boolean isWhitespaceGNUClasspath(char ch) {	  
		int attr = (int) ch;
		return ((((1 << (attr & TYPE_MASK))
				& ((1 << SPACE_SEPARATOR)
						| (1 << LINE_SEPARATOR)
						| (1 << PARAGRAPH_SEPARATOR))) != 0)
						&& (attr & NO_BREAK_MASK) == 0)
						|| (ch <= '\u001F' && ((1 << ch)
								& ((1 << '\t')
										| (1 << '\n')
										| (1 << '\u000B')
										| (1 << '\u000C')
										| (1 << '\r')
										| (1 << '\u001C')
										| (1 << '\u001D')
										| (1 << '\u001E')
										| (1 << '\u001F'))) != 0);
	}

	/**
	 * Determines if the specified character is white space according to Java.
	 *
	 * @param   ch the character to be tested.
	 * @return  <code>true</code> if the character is a Java whitespace
	 *          character; <code>false</code> otherwise.
	 * @see     java.lang.Character#isSpaceChar(char)
	 */
	public static boolean isWhitespace(char c) {
		int ci = (int) c;
		switch (ci) {
		case 9:		// Horizontal Tab 
		case 10:	// Newline
		case 11:	// Vertical Tab
		case 12:	// New Page
		case 13:	// Carriage Return
		case 28:	// File separator
		case 29:	// Group separator
		case 30:	// Record separator
		case 31:	// Unit separator	
		case 32:	// Space
			return true;
		default:
			return false;
		}
	}

	/**
	 * Read and return a Scheme expression, or EOF.
	 */
	public Object read() {
		try {
			Object token = nextToken();
			if (token == "(") {
				return readTail();
			} else if (token == ")") {
				System.out.println("WARN: Extra ')' ignored.");
				return read();
			} else {
				return token;
			} // End of the if - else
		} catch (IOException e) {
			System.out.println("WARN: On input, exception: " + e);
			return EOF;
		} // End try - catch
	}

	private Object readTail() throws IOException {
		Object token = nextToken();
		System.out.println("trace: readTail(): " + token);
		if (token == EOF) {
			final String msg = "ERROR: readTail() - EOF during read.";
			System.err.println(msg);
			throw (new RuntimeException(msg));
		} else if (token == ")") {
			return null;
		} else if (token == ".") {
			Object result = read();
			token = nextToken();
			if (token != ")") {
				System.out.println("WARN: Missing ')'? Received " + token + " after .");
			}
			return result;
		} else {
			tokenStack.push(token);
			return SchemeUtil.cons(read(), readTail());
		}
	}
	
	/**
	 * Collect the set of characters from the input stream until whitespace or
	 * one of the language tokens is found.
	 * 
	 * @param o_ch
	 * @throws IOException
	 */
	private void buildGenericToken(final int o_ch) throws IOException {
		int ch = o_ch;
		do {
			// Build alpha numeric, atom/symbol characters/tokens into the buffer
			buff.append((char) ch);
			ch = inputReader.read();
		} while (!Character.isWhitespace((char) ch)
				&& (ch != -1)
				&& (ch != TOK_LEFT_PAREN)  && (ch != TOK_RIGHT_PAREN) 
				&& (ch != TOK_SINGLE_QUOT) && (ch != TOK_SEMICOLON)
				&& (ch != TOK_DOUBLE_QUOT) && (ch != TOK_COMMA) 
				&& (ch != TOK_BACK_QUOT)); // End of do - while
		
		// Push a language token onto the character stack
		charStack.push(new Character((char) ch));
	}
	
	private Object nextToken() throws IOException {
		int ch;

		// See if we should re-use a pushed char or token
		// Task 1: Pop the token and character stacks
		if (!this.tokenStack.empty() && (this.tokenStack.peek() != null)) {
			return this.tokenStack.pop();
		} else if (!this.charStack.empty() && (this.charStack.peek() != null)) {
			ch = ((Character) this.charStack.pop()).charValue();
		} else {
			ch = inputReader.read();
		}

		// Ignore whitespace
		// Task 2: Check for and ignore whitespace
		while (isWhitespace((char) ch)) {
			ch = inputReader.read();
		}
		System.out.println("trace: nextToken() -> " + (char) ch + " $" + ch);

		// See what kind of non-white character we got
		// Task 3: Check if the character is of various token types.
		switch (ch) {
			case -1:
				return EOF;
			case TOK_LEFT_PAREN:
				return "(";
			case TOK_RIGHT_PAREN:
				return ")";
			case TOK_SINGLE_QUOT:
				return "'";
			case TOK_BACK_QUOT:
				return "`";
			case TOK_SEMICOLON:
				// Comment: skip to end of line and then read next token
				while (ch != -1 && ch != '\n' && ch != '\r') {
					ch = inputReader.read();
				}
				return nextToken();
			case TOK_DOUBLE_QUOT:
				// Strings are represented as char[]
				buff.setLength(0);
				while ((ch = inputReader.read()) != '"' && ch != -1) {
					buff.append((char) ((ch == '\\') ? inputReader.read() : ch));
				}
				if (ch == -1) {
					System.out.println("WARN: EOF inside of a string.");
				}
				return buff.toString().toCharArray();
			case '#':
				// Begin new switch statement, next set of characters
				switch (ch = inputReader.read()) {
					case 't':
					case 'T':
						return Boolean.TRUE;
					case 'f':
					case 'F':
						return Boolean.FALSE;
					default:
						System.out.println("WARN: #" + ((char) ch)
								+ " not recognized, ignored.");
						return nextToken();
				} // End of Switch
			default:
				buff.setLength(0);				
				int c = ch;
				buildGenericToken(ch);
				// Try potential numbers, but catch any format errors.
				if (c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9')) {
					try {
						// Number type is currently in the buffer queue
						return new Double(buff.toString());
					} catch (NumberFormatException e) {
						;
					}
				} // End of If
				return buff.toString().toLowerCase();
		} // End of the Switch
	}

}
