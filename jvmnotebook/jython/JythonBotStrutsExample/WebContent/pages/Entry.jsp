<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://struts.apache.org/tags-bean" prefix="bean" %>


<html:html>
<head>
<html:base/>
 <link href="/botspiritlistbeta/stylesheets/scaffold.css" media="screen" rel="Stylesheet" type="text/css" />
</head>
<body bgcolor="white">
<div style="margin-left: 60px; margin-top: 60px;">

<h3>Enter Information</h3>
<p>

 <html:form action="/actions/simple.do" >
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
 
 <p>
 <html:submit value="Submit" />
 </html:form>
 
</div>
</body>
</html:html>
