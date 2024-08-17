/* 
 * Created on Sep 25, 2006
 * 
 */
package org.spirit.forms;

import org.apache.struts.action.ActionForm;

public class CityStateForm extends ActionForm {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1533358640187914718L;
	private String city = null;
	private String state = null;
	
	/**
	 * @return
	 */
	public String getCity() {
		return city;
	}

	/**
	 * @return
	 */
	public String getState() {
		return state;
	}

	/**
	 * @param string
	 */
	public void setCity(String string) {
		city = string;
	}

	/**
	 * @param string
	 */
	public void setState(String string) {
		state = string;
	}

}
