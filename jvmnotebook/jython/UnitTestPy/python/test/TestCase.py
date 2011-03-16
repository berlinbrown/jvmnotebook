#
# TestCase.py
#
from org.jdom import Document
from org.jdom import Element
from org.jdom.input import SAXBuilder

from org.xml.sax import InputSource

from javax.xml.parsers import DocumentBuilder
from javax.xml.parsers import DocumentBuilderFactory

from java.io import File
from java.io import FileInputStream

#
# The user bean contains attributes that define the
# user behavior including value,id,email
#
class User:
    def __init__(self):
         self.value = None
         self.id = None
         self.email = None
        
#
# Test the basic JDOM XML functionality
#
# @author: Berlin Brown
# @version: $Id$
#
class TestCase:

    def __init__(self, mainurl=None):
        self.url = mainurl
        
    def processXML(self,doc):
        
        root = doc.getRootElement()
        docType = root.getName()        
        allusers = root.getChildren()
        for node in allusers:
            print node
           
    def run(self):
        print "** Processing : ", self.url
        
        f = File(self.url)
        builder = SAXBuilder()
        document = builder.build(FileInputStream(f))

        if document == None:
            print "Err: Invalid Document"
        else:
            self.processXML(document)
        
        

        
        
