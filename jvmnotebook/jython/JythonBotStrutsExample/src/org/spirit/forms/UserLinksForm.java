/* 
 * Created on Sep 25, 2006
 * 
 */
package org.spirit.forms;

import org.apache.struts.action.ActionForm;

public class UserLinksForm extends ActionForm {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8181978340987553608L;
	private String url = null;
	private String keywords = null;
	private String description = null;
	private String id = null;
	
	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @return
	 */
	public String getKeywords() {
		return keywords;
	}

	/**
	 * @return
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * @param string
	 */
	public void setDescription(String string) {
		description = string;
	}

	/**
	 * @param string
	 */
	public void setKeywords(String string) {
		keywords = string;
	}

	/**
	 * @param string
	 */
	public void setUrl(String string) {
		url = string;
	}

}

