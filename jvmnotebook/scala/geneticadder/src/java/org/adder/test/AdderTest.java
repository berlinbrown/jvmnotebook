package org.adder.test;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import junit.framework.TestCase;

import org.adder.EquationGenome;
import org.adder.Genome;
import org.adder.Population;

public class AdderTest extends TestCase {

    /**
     * The Random Seem.
     */
    public static final Random TheSeed = new Random(System.currentTimeMillis());
    
    public void testSample() {
    
        for (int i = 0; i < 20; i++) {
            final int res = EquationGenome.nextInt(TheSeed, 1, 20);
            System.out.println("Res=>" + res);
        }
        System.out.println("----");
        for (int i = 0; i < 20; i++) {
            final int res = EquationGenome.nextInt(TheSeed, 10, 20);
            System.out.println("Res=>" + res);
        }
        
    }
    
    public void testRemoveRange() {
        
        final List<Genome> elements = new ArrayList<Genome>();
        final EquationGenome g1 = new EquationGenome();
        final EquationGenome g2 = new EquationGenome();
        final EquationGenome g3 = new EquationGenome();
        final EquationGenome g4 = new EquationGenome();
        final EquationGenome g5 = new EquationGenome();
        
        elements.add(g1);
        elements.add(g2);
        elements.add(g3);
        elements.add(g4);
        elements.add(g5);        
       
        for (final java.util.Iterator<Genome> it = elements.iterator(); it.hasNext();) {
            
            System.out.println(it.next());
            
        } // End of for
        
        // Test remove
        System.out.println("-------");
        Population.removeRange(elements, 2, 3);
        
        for (final java.util.Iterator<Genome> it = elements.iterator(); it.hasNext();) {
            
            System.out.println(it.next());
            
        } // End of for

        
    }
    
} // End of the Class //
