package org.adder.test;

import junit.framework.TestSuite;
import junit.textui.TestRunner;

public class AdderTestSuite {

    public static final TestSuite suite() {
        
        final TestSuite suite = new TestSuite();
        suite.addTestSuite(AdderTest.class);
        return suite;
        
    }
    
    public static void main(final String [] args) {
        
        System.out.print("Running Tests");
        TestRunner.run(suite());
        System.out.print("Done");
        
    }
    
}
