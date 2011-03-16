/*
 * Berlin Brown
 * HibernateUtilPlugin.java
 * 
 * Tested for hibernate 3.1
 */
package org.spirit.util;
 
import java.net.URL;
import javax.servlet.ServletException;

import org.hibernate.MappingException;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.struts.action.ActionServlet;
import org.apache.struts.action.PlugIn;
import org.apache.struts.config.ModuleConfig;

public class HibernateUtilPlugin implements PlugIn {

	private Configuration config;
	private SessionFactory factory;
	private String path = "/hibernate.cfg.xml";

	private static Class className = HibernateUtilPlugin.class;
	public static final String KEY_NAME = className.getName();

	private static Log log = LogFactory.getLog(className);

	public void setPath(String path) {
		this.path = path;
	}

	public void init(ActionServlet servlet, ModuleConfig modConfig) throws ServletException {

		try {
			URL url = HibernateUtilPlugin.class.getResource(path);
			System.out.println("Found hibernate URL at=" + url);
			System.out.println("KeyName=" + KEY_NAME);			
			config = new Configuration().configure();
			factory = config.buildSessionFactory();
			servlet.getServletContext().setAttribute(KEY_NAME, factory);
			
			log.info("Hibernate plugin setup complet=");
			
		} catch (MappingException e) {
			log.error("mapping error", e);
			throw new ServletException();

		} catch (Exception e) {
			log.error("hibernate error", e);
			throw new ServletException();
		}

	}

	public void destroy() {
		try {
			factory.close();
		} catch (Exception e) {
			log.error("unable to close factory", e);
		}
	}
}
