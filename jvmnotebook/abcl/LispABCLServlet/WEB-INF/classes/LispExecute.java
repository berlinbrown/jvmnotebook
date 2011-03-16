/**
 * Berlin Brown
 */
import java.io.*;
import java.text.*;

import java.util.List;
import java.util.ArrayList;

import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispThread;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.Symbol;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Load;
import org.armedbear.lisp.Packages;

/**
 * Lisp Execute 
 * @author Berlin Brown 
 */
public class LispExecute extends HttpServlet {
	
	Interpreter interpreter;
	Servlet lispServlet = null;
	LispObject debuggerHook = null;
	
	/**
	 * Init.
	 */
	public void init(ServletConfig config) throws ServletException {
		
		super.init(config);
		interpreter = Interpreter.getInstance();
		if (interpreter == null) {
			log("making new interpreter");
			interpreter = Interpreter.createInstance();
			Interpreter.initializeLisp(false);
		}        
		try {
			LispThread thread = LispThread.currentThread();
			org.armedbear.lisp.Package swank = Packages.findPackage("SWANK");			
			if (swank != null) {
				debuggerHook = swank.findAccessibleSymbol("SWANK-DEBUGGER-HOOK").getSymbolFunction();
				log("using swank debugger hook");
			} else {
				Symbol dbgrhkfunSym = Lisp.PACKAGE_SYS.findAccessibleSymbol("%DEBUGGER-HOOK-FUNCTION");
				debuggerHook = dbgrhkfunSym.getSymbolFunction();
				log("using throwing debugger hook");
			}
			thread.bindSpecial(Lisp._DEBUGGER_HOOK_, debuggerHook);
			Load.load(config.getServletContext().getRealPath("WEB-INF/lisp/servlet-loader.lisp"));
			
			JavaObject svletJO = (JavaObject)Lisp.PACKAGE_CL_USER.findAccessibleSymbol("*SERVLET*").getSymbolValue();        	
			if (svletJO != null)
				lispServlet = (Servlet)svletJO.getObject();
		} catch(ConditionThrowable e) {
			try {
				System.out.print(e.getCondition().writeToString());
			} catch (ConditionThrowable e2)
			{}
		}
	}
	
	/**
	 * Do Get
	 */
	public void doGet(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {
		if (interpreter == null || lispServlet == null) {
			response.setContentType("text/html");
			PrintWriter out = response.getWriter();
			out.println("<html><body><h1>error:");
			if (interpreter == null)
				out.println("no interpreter");
			else out.println("no servlet");
			out.println("</h1></body></html>");
		} else {
			LispThread thread = LispThread.currentThread();
			thread.bindSpecial(Lisp._DEBUGGER_HOOK_, debuggerHook);
			lispServlet.service(request, response);
		}
	}
	
	/**
	 * Do Post
	 */
	public void doPost(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {
		
		if (interpreter == null || lispServlet == null) {
			response.setContentType("text/html");
			PrintWriter out = response.getWriter();
			out.println("<html><body><h1>error:");
			if (interpreter == null)
				out.println("no interpreter");
			else out.println("no servlet");
			out.println("</h1></body></html>");
		} else {
			LispThread thread = LispThread.currentThread();
			thread.bindSpecial(Lisp._DEBUGGER_HOOK_, debuggerHook);
			lispServlet.service(request, response);
		}
	}	
	
}



