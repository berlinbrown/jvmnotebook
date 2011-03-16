/*
 * Original Created on Mar 21, 2005
 *
 * $Id$
 * 
 * Author: Berlin Brown
 * 
 * Original Package: org.spirit.jy
 * Original Project: JyNetwork    
 * 2:59:25 PM
 * 
 * Revisions:
 * 
 * ----------  -------------------
 * 9/21/2006	Revisiting Application
 * 
 */
package org.spirit.jy;

import java.io.IOException;
import java.io.File;

import java.util.Enumeration;
import javax.servlet.ServletException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.python.util.PythonInterpreter;
import org.python.core.PySystemState;
import org.python.core.Py;
import org.python.core.PyException;
import org.python.core.PyString;

import java.util.Properties;
import java.util.Hashtable;

import java.io.PrintWriter;

/**
 * Python Servlet Setup Utilities;  Setup your web application
 * environment for use with jython.
 * 
 * This application was tested with WSAD 5.1.2.  Make sure that you created
 * a dynamic web-application project such that you have the following directory
 * structure (modify for your appropriate Application Server):
 *  JavaSource
 * 		org.spirit.jy
 * 			NewPyServlet.java
 *  WebContent
 * 		__ META-INF
 * 		__ WEB-INF
 * 			____ classes
 * 			____ lib
 * 				web.xml
 * 		SimpleAction.py (at root in WebContent directory)
 *  
 * 	<p><b>External Dir Structure Needed (jython/python home)</b>
 *  <p>
 *  python.home = C:/projects2/Downloads5/jy
 *  	contains 'jython.jar'
 * 
 * @author berlin.brown
 *
 */
public class NewPyServlet extends HttpServlet {

	private static Log logger = LogFactory.getLog(NewPyServlet.class.getName());

	private PythonInterpreter interp;
	private Hashtable cache = new Hashtable();
	private String rootPath;
	
	/**
	 * Hardcoded absolute path to your JYTHON home
	 */
	private String PY_HOME_ABSOLUTE  = "C:/projects2/Downloads5/jy";

	///==============================================================
	///
	/// ** Methods **
	///
	///==============================================================   
	public void init() {

		rootPath = getServletContext().getRealPath("/");
		if (!rootPath.endsWith(File.separator))
			rootPath += File.separator;

		Properties props = new Properties();

		// Context parameters
		ServletContext context = getServletContext();
		Enumeration e = context.getInitParameterNames();
		while (e.hasMoreElements()) {
			String name = (String) e.nextElement();
			props.put(name, context.getInitParameter(name));
		} /// end of the while //

		// Config parameters
		e = getInitParameterNames();
		while (e.hasMoreElements()) {
			String name = (String) e.nextElement();
			props.put(name, getInitParameter(name));
		} /// end of while

		if ((props.getProperty("python.home") == null) && (System.getProperty("python.home") == null)) {
			
			//props.put("python.home", rootPath + "WEB-INF" + File.separator + "lib");
			props.put("python.home", PY_HOME_ABSOLUTE);
			
		} // end of if

		PythonInterpreter.initialize(System.getProperties(), props, new String[0]);
		reset();
		PySystemState sys = Py.getSystemState();
		PySystemState.add_package("javax.servlet");
		PySystemState.add_package("javax.servlet.http");
		PySystemState.add_package("javax.servlet.jsp");
		PySystemState.add_package("javax.servlet.jsp.tagext");

		PySystemState.add_classdir(rootPath + "WEB-INF" + File.separator + "classes");
		
		// May be deprecated
		// PySystemState.add_extdir(rootPath + "WEB-INF" + File.separator + "lib", true);
		PySystemState.add_extdir(rootPath + "WEB-INF" + File.separator + "lib");

	} /// end of the method //

	public void reset() {

		interp = new PythonInterpreter(null, new PySystemState());
		cache.clear();
		PySystemState sys = Py.getSystemState();
		sys.path.append(new PyString(rootPath));

		String modulesDir = rootPath + "WEB-INF" + File.separator + "jython";
		sys.path.append(new PyString(modulesDir));

	} /// end of the method //

	///
	/// Get and Post Methods
	///
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		doPost(request, response);

	} /// end of the method //

	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		String spath = ((HttpServletRequest) request).getServletPath();
		if (spath == null || spath.length() == 0) {
			spath = ((HttpServletRequest) request).getPathInfo();
		} /// end of the if //                             
		try {

			///
			/// This will get calling servlet name
			/// For now assume, the root directory is webapps/ROOT
			///
			String rpath = getServletContext().getRealPath(spath);
			String rootPath = getServletContext().getRealPath("/");			
			interp.execfile(rootPath + "/" + "DefaultHandler.py");
			interp.set("req", request);
			interp.set("res", response);
			interp.set("log", logger);
			interp.exec("handl = DefaultHandler(log)");
			interp.exec("handl.doPost(req, res)");

		} catch (PyException e) {
			logger.error(e);
			throw new ServletException("## ERR ## Could not create " + "Jython servlet - " + e.getMessage());

		} /// end of the try - catch //

	} /// end of the method //

} /// end of the class //
