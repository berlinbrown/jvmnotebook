/*
 * This file is part of JGAP.
 *
 * JGAP offers a dual license model containing the LGPL as well as the MPL.
 *
 * For licensing information please see the file license.txt included with JGAP
 * or have a look at the top of class org.jgap.Chromosome which representatively
 * includes the JGAP license policy applicable for any file delivered with JGAP.
 */
package examples.gp.monalisa.core;

import org.jgap.gp.*;

/**
 * Initializes GP programs properly when they are created.
 *
 * @author Klaus Meffert
 * @since 3.4
 */
public class InitStrategy
    implements IGPInitStrategy {
  /** String containing the CVS revision. Read out via reflection!*/
  private final static String CVS_REVISION = "$Revision: 1.1 $";

  /**
   * Initializes a chromosome within a GP program before a random creation of
   * the (rest of the) program is executed.
   *
   * @param a_chrom the chromosome within the GP program to create randomly
   * @param a_chromNum index of the chromosome with the GP program
   * @return the CommandGene to use as first node, or null if random selection
   * @throws Exception
   *
   * @author Klaus Meffert
   * @since 3.4
   */
  public CommandGene init(IGPChromosome a_chrom, int a_chromNum)
      throws Exception {
    if (a_chromNum == 0) {
      CommandGene gene = DrawingProblem.SUBPROGRAM;
      return gene;
    }
    return null;
  }
}
