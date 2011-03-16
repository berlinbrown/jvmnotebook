<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://struts.apache.org/tags-bean" prefix="bean" %>
<%@ taglib uri="http://struts.apache.org/tags-logic" prefix="logic" %>

<html:html>
<head>
<html:base/>
 <link href="/botspiritlistbeta/stylesheets/scaffold.css" media="screen" rel="Stylesheet" type="text/css" />
</head>
<body bgcolor="white">
<div style="margin-left: 60px; margin-top: 60px;">
<h3>List of Current Links</h3>

* <html:link action="/Entry.do">Add New Link</html:link>
<p>

<html:messages id="msglocal" message="true" property="messageslist">
 <bean:write name="msglocal"/>
</html:messages>

<table>
<logic:iterate id="element" name="bot.user.links" scope="session"
				indexId="index" >
	<tr>			
	 <td>
	 	<bean:write name="element" property="mainUrl"/>				
	 </td>
	 <td>
	 	<bean:write name="element" property="keywords"/>
	 </td>
	 <%-- Print the CRUD action links --%>
	 <%-- Request query string will include the 'id' for each user link --%>
	 <td>
	 	| <html:link action="/actions/view.do" paramId="id"
	 		paramName="element" paramProperty="id">View Form</html:link>
	 </td>
	 <td>
	 	| <html:link action="/actions/edit.do" paramId="id"
	 		paramName="element" paramProperty="id">Edit Form</html:link>
	 </td>
	 <td>
	 	| <html:link action="/actions/delete.do" paramId="id"
	 		paramName="element" paramProperty="id" >Delete</html:link>
	 </td>
	</tr>
</logic:iterate>
</table>
</div>
</body>
</html:html>
