/*
 * Created on Oct 4, 2006
 *
 */
package org.spirit.util;

import java.io.File;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Properties;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.struts.action.ActionServlet;
import org.apache.struts.action.PlugIn;
import org.apache.struts.config.ModuleConfig;
import org.python.core.Py;
import org.python.core.PyString;
import org.python.core.PySystemState;
import org.python.util.PythonInterpreter;

/**
 * This class will initialize hibernate and bind SessionFactory in JNDI at the 
 * time of application and startup and unbind it from JNDI at the time of application
 * shutdown
 * 
 * @author Berlin Brown
 */
public class JythonUtilPlugin implements PlugIn, java.io.Serializable {

	/**
	 * Serial Version UID.
	 */
	private static final long serialVersionUID = -2075959387140050283L;

	private String name;

	public static final String PLUGIN_NAME_KEY = JythonUtilPlugin.class.getName();

	/** 
	 * Logging instance.
	 */
	private static Log log = LogFactory.getLog(JythonUtilPlugin.class);

	private PythonInterpreter interp;

	private Hashtable cache = new Hashtable();

	private String rootPath;

	/**
	 * Hardcoded absolute path to your JYTHON home
	 */
	private String PY_HOME_ABSOLUTE = "C:/Jython2.2a";

	///==============================================================
	///
	/// ** Methods **
	///
	///==============================================================   

	public void reset() {

		interp = new PythonInterpreter(null, new PySystemState());
		cache.clear();
		PySystemState sys = Py.getSystemState();
		sys.path.append(new PyString(rootPath));

		String modulesDir = rootPath + "WEB-INF" + File.separator + "jython";
		sys.path.append(new PyString(modulesDir));
		
		// Needed in order python to recognize classes in the class directory
		String classDir = rootPath + "WEB-INF" + File.separator + "classes";
		sys.path.append(new PyString(classDir));

	} /// end of the method //

	/**
	 * This method will be called at the time of application shutdown.
	 */
	public void destroy() {
		log.info("Entering JythonPlugIn.destroy()");
		//Put hibernate cleanup code here
		log.info("Exiting JythonPlugIn.destroy()");
	}

	/**
	 * This method will be called at the time of application startup.
	 */
	public void init(ActionServlet actionServlet, ModuleConfig config) throws ServletException {

		log.info("Entering JythonPlugIn.init()");
		log.info("Value of init parameter " + getName());
		log.info("Exiting JythonPlugIn.init()");

		ServletContext context = null;
		context = actionServlet.getServletContext();
		context.setAttribute(PLUGIN_NAME_KEY, this);

		rootPath = context.getRealPath("/");
		if (!rootPath.endsWith(File.separator))
			rootPath += File.separator;

		Properties props = new Properties();

		// Context parameters		
		Enumeration e = context.getInitParameterNames();
		while (e.hasMoreElements()) {
			String name = (String) e.nextElement();
			props.put(name, context.getInitParameter(name));
		} /// end of the while //

		// Config parameters
		/*
		e = getInitParameterNames();
		while (e.hasMoreElements()) {
			String name = (String) e.nextElement();
			props.put(name, getInitParameter(name));
		} /// end of while
		*/
		if ((props.getProperty("python.home") == null) && (System.getProperty("python.home") == null)) {

			//props.put("python.home", rootPath + "WEB-INF" + File.separator + "lib");
			props.put("python.home", PY_HOME_ABSOLUTE);

		} // end of if

		PythonInterpreter.initialize(System.getProperties(), props, new String[0]);
		reset();
		//PySystemState sys = Py.getSystemState();
		PySystemState.add_package("javax.servlet");
		PySystemState.add_package("javax.servlet.http");
		PySystemState.add_package("javax.servlet.jsp");
		PySystemState.add_package("javax.servlet.jsp.tagext");

		PySystemState.add_classdir(rootPath + "WEB-INF" + File.separator + "classes");

		// May be deprecated
		// PySystemState.add_extdir(rootPath + "WEB-INF" + File.separator + "lib", true);
		PySystemState.add_extdir(rootPath + "WEB-INF" + File.separator + "lib");
		PySystemState.add_extdir(rootPath + "WEB-INF" + File.separator + "classes",true);
	}
	
	public PythonInterpreter getInterpreter() {

		if (this.interp == null) {
			reset();
		}
		return this.interp;
	}
	
	public String getInternalName() {
		return "JythonUtilPlugin";
	}
	
	public String getName() {
		return name;
	}

	public void setName(String string) {
		name = string;
	}
}
