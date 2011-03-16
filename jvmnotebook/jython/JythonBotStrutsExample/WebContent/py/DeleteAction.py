##
## Berlin Brown
## 9/25/2006

from java.lang import Long

from org.apache.struts.action import Action
from org.apache.struts.action import ActionMessages
from org.apache.struts.action import ActionMessage

from org.spirit.bean.impl import BotListUserLink
from org.spirit.actions import SimpleAction

from org.spirit.util import HibernateUtilPlugin
from org.spirit.util import BotListConsts

class DeleteAction (Action):

	def execute(self, mapping, form, request, response):
				
		# Extract the Hibernate Session Object
		servlet = self.getServlet()
		context = servlet.getServletContext()
		httpSession = request.getSession()

		curId = request.getParameter("id")
		print "Link ID=", curId
		
		# Removed due to Websphere/Hibernate classloader/classcast issues
		objFactory = context.getAttribute(HibernateUtilPlugin.KEY_NAME)
		
		# We have the needed object, we just need to invoke the factory method
 		# curSess = SimpleAction.invokeVoidContextObject(objFactory, "org.hibernate.SessionFactory", "openSession")
		curSess = objFactory.openSession()		
		tx = curSess.beginTransaction()
		
		# Extract the single entity (long line for query)
	        queryRes = curSess.createQuery("select l from org.spirit.bean.impl.BotListUserLink as l where l.id = :linkid")
		queryRes.setLong("linkid", Long(curId).longValue())
		resLink = queryRes.uniqueResult()
		curSess.delete(resLink)
		httpSession.setAttribute(BotListConsts.BOT_SINGLE_LINK, resLink)
		tx.commit()

		linksMsg = "Deleted Entry"
		# Set the confirmation message
		messages = ActionMessages()
		msg = ActionMessage("userLink.info.list", linksMsg)
		messages.add("messagesconfirm", msg)
		self.saveMessages(request, messages)
		
		return mapping.findForward("success")
	
# End of the File        
