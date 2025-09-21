/*
 * Created on Sep 22, 2006
 * 
 */
package org.spirit.jy;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class DummyHttpResponse implements HttpServletResponse {

	/**
	 * @see javax.servlet.http.HttpServletResponse#addCookie(javax.servlet.http.Cookie)
	 */
	public void addCookie(Cookie arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#containsHeader(java.lang.String)
	 */
	public boolean containsHeader(String arg0) {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeURL(java.lang.String)
	 */
	public String encodeURL(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeRedirectURL(java.lang.String)
	 */
	public String encodeRedirectURL(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeUrl(java.lang.String)
	 */
	public String encodeUrl(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#encodeRedirectUrl(java.lang.String)
	 */
	public String encodeRedirectUrl(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int, java.lang.String)
	 */
	public void sendError(int arg0, String arg1) throws IOException {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendError(int)
	 */
	public void sendError(int arg0) throws IOException {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#sendRedirect(java.lang.String)
	 */
	public void sendRedirect(String arg0) throws IOException {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setDateHeader(java.lang.String, long)
	 */
	public void setDateHeader(String arg0, long arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addDateHeader(java.lang.String, long)
	 */
	public void addDateHeader(String arg0, long arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setHeader(java.lang.String, java.lang.String)
	 */
	public void setHeader(String arg0, String arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addHeader(java.lang.String, java.lang.String)
	 */
	public void addHeader(String arg0, String arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setIntHeader(java.lang.String, int)
	 */
	public void setIntHeader(String arg0, int arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#addIntHeader(java.lang.String, int)
	 */
	public void addIntHeader(String arg0, int arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int)
	 */
	public void setStatus(int arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.http.HttpServletResponse#setStatus(int, java.lang.String)
	 */
	public void setStatus(int arg0, String arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#getCharacterEncoding()
	 */
	public String getCharacterEncoding() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletResponse#getOutputStream()
	 */
	public ServletOutputStream getOutputStream() throws IOException {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletResponse#getWriter()
	 */
	public PrintWriter getWriter() throws IOException {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentLength(int)
	 */
	public void setContentLength(int arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#setContentType(java.lang.String)
	 */
	public void setContentType(String arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#setBufferSize(int)
	 */
	public void setBufferSize(int arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#getBufferSize()
	 */
	public int getBufferSize() {
		//  Auto-generated method stub
		return 0;
	}

	/**
	 * @see javax.servlet.ServletResponse#flushBuffer()
	 */
	public void flushBuffer() throws IOException {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#resetBuffer()
	 */
	public void resetBuffer() {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#isCommitted()
	 */
	public boolean isCommitted() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.ServletResponse#reset()
	 */
	public void reset() {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#setLocale(java.util.Locale)
	 */
	public void setLocale(Locale arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletResponse#getLocale()
	 */
	public Locale getLocale() {
		//  Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletResponse#getContentType()
	 */
	public String getContentType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletResponse#setCharacterEncoding(java.lang.String)
	 */
	public void setCharacterEncoding(String arg0) {
		// TODO Auto-generated method stub
		
	}


}

