#
# $Id$
#
# TestCase for the User XML loader
#

import sys

from jylib import unittest
from jylib.unittest import TestCase

from com.motricity import UserLoader


class UserTest(TestCase):

    def __init__(self, name): 
        TestCase.__init__(self,name)
        self.check_registry()
        self.assertTrue = self.assert_

    def check_registry(self):
        if sys.registry['python.security.respectJavaAccessibility'] == 'true':
            print MESSAGE_REGISTRY
            sys.exit(0)
        
    def unitMain(self):
        unittest.main()
        
    def setUp(self):
        print "."

    def tearDown(self):
        print "_"

    def testValidDocument(self):
        xml1 = '''<?xml version="1.0" encoding="UTF-8" ?>
<users>
 <name value="Cat" id="7" />
</users>
'''
        user = UserLoader(None)        
        user.processDoc(xml1)
        expected = 2
        self.assertEquals(expected, user.size())
        
#
# End of Class
#

def suite():
    return unittest.makeSuite(UserTest)

if __name__ == '__main__':
  unittest.main()


        
