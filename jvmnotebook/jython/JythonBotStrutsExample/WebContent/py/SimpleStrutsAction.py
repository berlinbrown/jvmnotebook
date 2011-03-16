##
## Berlin Brown
## 9/25/2006

from org.apache.struts.action import Action
from org.apache.struts.action import ActionMessages
from org.apache.struts.action import ActionMessage

from org.spirit.bean.impl import BotListUserLink
from org.spirit.actions import SimpleAction

from org.spirit.util import HibernateUtilPlugin
from java.lang import Class
from java.lang.reflect import Constructor
from java.lang.reflect import Method

class SimpleStrutsAction (Action):

	def execute(self, mapping, form, request, response):
		print "Execute Simple Struts Action"
		print "Mapping GetParameter", mapping.getParameter()
		print "Get Path", mapping.getPath()
		
		print "Form Type", dir(form)
		url = form.url
		keywords = form.keywords
		description = form.description
		
		# Extract the Hibernate Session Object
		servlet = self.getServlet()
		context = servlet.getServletContext()
				
		# Removed due to Websphere/Hibernate classloader/classcast issues
		objFactory = context.getAttribute(HibernateUtilPlugin.KEY_NAME)
		
		# We have the needed object, we just need to invoke the factory method
 		# curSess = SimpleAction.invokeVoidContextObject(objFactory, "org.hibernate.SessionFactory", "openSession")
		curSess = objFactory.openSession()
		curSess.beginTransaction()
		
		link = BotListUserLink()			
		link.mainUrl = url
		link.description = description
		link.keywords = keywords
		curSess.save(link)
		curSess.getTransaction().commit()
		
		# Set the confirmation message
		messages = ActionMessages()
		msg = ActionMessage("userLink.info.save", "Data Saved")
		messages.add("messagesconfirm", msg)
		self.saveMessages(request, messages)
		
		return mapping.findForward("success")
	
# End of the File        
