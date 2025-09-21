/* 
 * Created on Sep 22, 2006
 * 
 * from org.spirit.jy import DummyHttpRequest as dumres
 * from org.spirit.jy import DummyHttpResponse as dumresp
 * 
 */
package org.spirit.jy;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletInputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

public class DummyHttpRequest implements HttpServletRequest {

	/**
	 * @see javax.servlet.http.HttpServletRequest#getAuthType()
	 */
	public String getAuthType() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getCookies()
	 */
	public Cookie[] getCookies() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getDateHeader(java.lang.String)
	 */
	public long getDateHeader(String arg0) {
		//  Auto-generated method stub
		return 0;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getHeader(java.lang.String)
	 */
	public String getHeader(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getHeaders(java.lang.String)
	 */
	public Enumeration getHeaders(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getHeaderNames()
	 */
	public Enumeration getHeaderNames() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getIntHeader(java.lang.String)
	 */
	public int getIntHeader(String arg0) {
		//  Auto-generated method stub
		return 0;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getMethod()
	 */
	public String getMethod() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getPathInfo()
	 */
	public String getPathInfo() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getPathTranslated()
	 */
	public String getPathTranslated() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getContextPath()
	 */
	public String getContextPath() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getQueryString()
	 */
	public String getQueryString() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getRemoteUser()
	 */
	public String getRemoteUser() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#isUserInRole(java.lang.String)
	 */
	public boolean isUserInRole(String arg0) {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getUserPrincipal()
	 */
	public Principal getUserPrincipal() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getRequestedSessionId()
	 */
	public String getRequestedSessionId() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getRequestURI()
	 */
	public String getRequestURI() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getRequestURL()
	 */
	public StringBuffer getRequestURL() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getServletPath()
	 */
	public String getServletPath() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getSession(boolean)
	 */
	public HttpSession getSession(boolean arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#getSession()
	 */
	public HttpSession getSession() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#isRequestedSessionIdValid()
	 */
	public boolean isRequestedSessionIdValid() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#isRequestedSessionIdFromCookie()
	 */
	public boolean isRequestedSessionIdFromCookie() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#isRequestedSessionIdFromURL()
	 */
	public boolean isRequestedSessionIdFromURL() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.http.HttpServletRequest#isRequestedSessionIdFromUrl()
	 */
	public boolean isRequestedSessionIdFromUrl() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.ServletRequest#getAttribute(java.lang.String)
	 */
	public Object getAttribute(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getAttributeNames()
	 */
	public Enumeration getAttributeNames() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getCharacterEncoding()
	 */
	public String getCharacterEncoding() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#setCharacterEncoding(java.lang.String)
	 */
	public void setCharacterEncoding(String arg0) throws UnsupportedEncodingException {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletRequest#getContentLength()
	 */
	public int getContentLength() {
		//  Auto-generated method stub
		return 0;
	}

	/**
	 * @see javax.servlet.ServletRequest#getContentType()
	 */
	public String getContentType() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getInputStream()
	 */
	public ServletInputStream getInputStream() throws IOException {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getParameter(java.lang.String)
	 */
	public String getParameter(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getParameterNames()
	 */
	public Enumeration getParameterNames() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getParameterValues(java.lang.String)
	 */
	public String[] getParameterValues(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getParameterMap()
	 */
	public Map getParameterMap() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getProtocol()
	 */
	public String getProtocol() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getScheme()
	 */
	public String getScheme() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getServerName()
	 */
	public String getServerName() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getServerPort()
	 */
	public int getServerPort() {
		//  Auto-generated method stub
		return 0;
	}

	/**
	 * @see javax.servlet.ServletRequest#getReader()
	 */
	public BufferedReader getReader() throws IOException {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getRemoteAddr()
	 */
	public String getRemoteAddr() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getRemoteHost()
	 */
	public String getRemoteHost() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#setAttribute(java.lang.String, java.lang.Object)
	 */
	public void setAttribute(String arg0, Object arg1) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletRequest#removeAttribute(java.lang.String)
	 */
	public void removeAttribute(String arg0) {
		//  Auto-generated method stub
		
	}

	/**
	 * @see javax.servlet.ServletRequest#getLocale()
	 */
	public Locale getLocale() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getLocales()
	 */
	public Enumeration getLocales() {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#isSecure()
	 */
	public boolean isSecure() {
		//  Auto-generated method stub
		return false;
	}

	/**
	 * @see javax.servlet.ServletRequest#getRequestDispatcher(java.lang.String)
	 */
	public RequestDispatcher getRequestDispatcher(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/**
	 * @see javax.servlet.ServletRequest#getRealPath(java.lang.String)
	 */
	public String getRealPath(String arg0) {
		//  Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletRequest#getLocalAddr()
	 */
	public String getLocalAddr() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletRequest#getLocalName()
	 */
	public String getLocalName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletRequest#getLocalPort()
	 */
	public int getLocalPort() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletRequest#getRemotePort()
	 */
	public int getRemotePort() {
		// TODO Auto-generated method stub
		return 0;
	}

}

