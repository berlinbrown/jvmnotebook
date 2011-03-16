/**
 * Berlin Brown
 * May 12, 2006
 */
package org.spirit.actions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.struts.action.Action;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;
import org.python.util.PythonInterpreter;
import org.spirit.util.JythonUtilPlugin;

/**
 * This is class is used by botverse.
 * @author Berlin Brown
 *
 */
public class DefaultActionHandler extends Action {
	
	/** 
	 * Logging instance.
	 */
    private static Log log = LogFactory.getLog(DefaultActionHandler.class);
    
    /**
     * InvokeContextObject.         
     */
    public static Object invokeVoidContextObject(Object objTarget, String fullClassName, String methodName)
    			throws Exception {
			// Get the python interpreter, reflection code added due to classloading issues.
			// Class clJy1 = objTarget.getClass().getClassLoader().loadClass(fullClassName);
			Class clJy1 = objTarget.getClass();
			Method m = clJy1.getMethod(methodName, null);
			log.info("Method: " + m);
			return m.invoke(objTarget, null);			
    }
    
	/**
	  * createContextInstance.
	  *      
	  */
	 public static Object createContextInstance(Object obj, String fullClassName)
				 throws Exception {
				 	
			 // Get the python interpreter, reflection code added due to classloading issues.			 
			 // Class clJy1 = obj.getClass().getClassLoader().loadClass(fullClassName);
			 // Class clJy1 = Class.forName(fullClassName);
			 Class clJy1 = obj.getClass().getClassLoader().loadClass(fullClassName);
			 Constructor ct = clJy1.getConstructor(null);			 
			 return ct.newInstance(null);		
	 }
    
    /**
     * pyExecute.
     */
    public ActionForward pyExecute(ActionMapping mapping,
            ActionForm form,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {
    	
    	String spath = ((HttpServletRequest) request).getServletPath();
    	if (spath == null || spath.length() == 0) {
    		spath = ((HttpServletRequest) request).getPathInfo();
    	} /// end of the if //     
    	
		ServletContext context = null;
		context = this.getServlet().getServletContext();
					
		Object objJyPlugin = context.getAttribute(JythonUtilPlugin.PLUGIN_NAME_KEY);
		log.info("Jy Executing=");
		
		// Get the python interpreter, reflection code added due to classloading issues.
		Class clJy1 = objJyPlugin.getClass().getClassLoader().loadClass("org.spirit.util.JythonUtilPlugin");
		Method m = clJy1.getMethod("getInterpreter", null);
		log.info("Method: " + m);
		PythonInterpreter interp = (PythonInterpreter) m.invoke(objJyPlugin, null);
		log.info(">>> Interpreter=" + interp);
		
    	String rootPath = context.getRealPath("/");
    	String pyActionClassInterp = rootPath + "/py/" + "SimpleStrutsAction.py";
    	String pyClassName = "SimpleStrutsAction";
    	
    	// Extract the parameter name which is associated with the jython class to invoke
    	// TODO: Design: this section could be modified to use a Factory of jython code to invoke
    	String actionParameter = mapping.getParameter();
    	if (actionParameter != null) {
    		if (actionParameter.equals("/actions/listlinks")) {
    			pyActionClassInterp = rootPath + "/py/" + "ListLinksAction.py";
    			pyClassName = "ListLinksAction";
    		} else if (actionParameter.equals("/actions/view")) {
    			
    			pyActionClassInterp = rootPath + "/py/" + "ViewAction.py";
    			pyClassName = "ViewAction";
    		} else if (actionParameter.equals("/actions/edit")) {
    			
    			pyActionClassInterp = rootPath + "/py/" + "EditAction.py";
    			pyClassName = "EditAction";
    		} else if (actionParameter.equals("/actions/editupdate")) {
    			
    			pyActionClassInterp = rootPath + "/py/" + "EditUpdateAction.py";
    			pyClassName = "EditUpdateAction";
    			
    		}  else if (actionParameter.equals("/actions/delete")) {
    			
    			pyActionClassInterp = rootPath + "/py/" + "DeleteAction.py";
    			pyClassName = "DeleteAction";
    		}
    		
    	}
    	
    	log.info(">>> Interpreting Jython Class=" + pyActionClassInterp);
    	interp.execfile(pyActionClassInterp);
    	
    	interp.set("mapping", mapping);
    	interp.set("form", form);
    	interp.set("req", request);
    	interp.set("res", response);
    	interp.set("servlet", this.getServlet());
    	interp.exec("simpl = " + pyClassName + "()");
    	interp.exec("simpl.setServlet(servlet)");
    	interp.exec("actionForward = simpl.execute(mapping, form, req, res)");
    	ActionForward forward = (ActionForward) interp.get("actionForward").__tojava__(org.apache.struts.action.ActionForward.class);
    	log.info("--->" + forward);        
    	return forward;
    }
           
	public ActionForward execute(ActionMapping mapping,
            ActionForm form,
            HttpServletRequest request,
            HttpServletResponse response) throws Exception {		
		return pyExecute(mapping, form, request,response);
	}

}
