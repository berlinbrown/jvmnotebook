#
# $Id$
#
# Berlin Brown
#-----------------------------------------------------------
#
# Sisc and Python
# 
#-----------------------------------------------------------

import sys

from test.jylib import getopt
from test.TestCase import TestCase
from test.UserTest import UserTest

from junit.framework import Test
from junit.framework import TestSuite
from junit.textui import TestRunner
            
#///--------------------------------------------------------
#/// Usage and Main
#///--------------------------------------------------------
class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg

def testsuite():
    UserTest.suite()

def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            # Run with 'Main.py -o SOME_OPTION'
            opts, args = getopt.getopt(argv[1:], "oz", ["option","ztest"])

            if ((len(opts) == 0) or (len(args) == 0)):
                raise Usage("Invalid Args - too few args")

            #//
            #// Based on the option - decide what class we need
            #//
            #// example: args[0] == 'v':
            #// "Directory"
            #//
            if opts[0][0] == '-o':

                # Run a simple demo python application
            
                print "** running"
                test = TestCase(args[0])
                test.run()
                print "** done"

            elif opts[0][0] == '-z':

                # Run the test cases
                testsuite()                
                                                                    
        except getopt.error, msg:
            raise Usage(msg)
         
    except Usage, err:
        print >>sys.stdout, err.msg
        print >>sys.stdout, "Use the -o/z[--option] to select a Utility program"
        print >>sys.stdout, "----------------------------------------"
        print >>sys.stdout, "  Use Main.py -o FILENAME"
        print >>sys.stdout
        print >>sys.stdout, "  see the jy.sh and run.sh scripts"
        print >>sys.stdout, "----------------------------------------"
        return -1

if __name__ == '__main__':
    sys.exit(main())
