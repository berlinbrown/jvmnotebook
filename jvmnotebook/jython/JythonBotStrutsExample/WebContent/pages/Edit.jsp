<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://struts.apache.org/tags-bean" prefix="bean" %>

<html:html>
<head>
<html:base/>
 <link href="/botspiritlistbeta/stylesheets/scaffold.css" media="screen" rel="Stylesheet" type="text/css" />
</head>
<body bgcolor="white">
<div style="margin-left: 60px; margin-top: 60px;">

<h3>Confirmed, message received</h3>

<P>
Message=
<br>
<html:messages id="msglocal" message="true" property="messagesconfirm">
 <bean:write name="msglocal"/>
</html:messages>

<p>
<table>
 <tr>
   <td>
   	<b>Link</b>
   </td>
   <td>
   	<bean:write name="bot.user.link" scope="session" property="mainUrl"/>	
   </td>
 </tr>
</table>

<p>
 <html:form action="/actions/editupdate.do" >
 <table>
  <tr>
  <td>Main Url:</td>
  <td><html:text property="url" /></td>
  </tr>
  <tr>
   <td>Keywords:</td>
   <td><html:text property="keywords" /></td>
  </tr>
  <tr>
	 <td>Description:</td>
	 <td><html:text property="description" /></td>
  </tr>  
 </table>
 <html:hidden property="id" />
  <p>
  <html:submit value="Update" />
 
 </html:form>

  <br>
 <html:link action="/actions/listlinks.do">View All Links</html:link>

</div>
</body>
</html:html>
