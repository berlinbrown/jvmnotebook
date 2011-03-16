##
## Berlin Brown
## 9/25/2006
## Note, the following code was interpreted with Jython.  If you need to 
## convert this to java, just add 'new' and add the semicolons appropriately.
##

from org.apache.struts.action import Action
from org.apache.struts.action import ActionMessages
from org.apache.struts.action import ActionMessage

from org.spirit.bean.impl import BotListUserLink
from org.spirit.actions import SimpleAction

from org.spirit.util import HibernateUtilPlugin
from org.spirit.util import BotListConsts

from java.lang import Class
from java.lang.reflect import Constructor
from java.lang.reflect import Method

class ListLinksAction (Action):

	def execute(self, mapping, form, request, response):
				
		# Extract the Hibernate Session Object
		servlet = self.getServlet()
		context = servlet.getServletContext()
		httpSession = request.getSession()
		
		# Removed due to Websphere/Hibernate classloader/classcast issues
		objFactory = context.getAttribute(HibernateUtilPlugin.KEY_NAME)
		
		# We have the needed object, we just need to invoke the factory method
 		# curSess = SimpleAction.invokeVoidContextObject(objFactory, "org.hibernate.SessionFactory", "openSession")
		curSess = objFactory.openSession()		
		curSess.beginTransaction()
		
		# List the links
		result = curSess.createQuery("from org.spirit.bean.impl.BotListUserLink").list()
		curSess.getTransaction().commit()		
		httpSession.setAttribute(BotListConsts.BOT_USER_LINKS, result)

		totl = 0
		if result:
			totl = result.size()
		
		linksMsg = "Links Found %s" % totl
		# Set the confirmation message
		messages = ActionMessages()
		msg = ActionMessage("userLink.info.list", linksMsg)
		messages.add("messageslist", msg)
		self.saveMessages(request, messages)
		
		return mapping.findForward("success")
	
# End of the File        
