/*
 * This file is part of JGAP.
 *
 * JGAP offers a dual license model containing the LGPL as well as the MPL.
 *
 * For licensing information please see the file license.txt included with JGAP
 * or have a look at the top of class org.jgap.Chromosome which representatively
 * includes the JGAP license policy applicable for any file delivered with JGAP.
 */
package examples.monalisa.core;

import java.util.*;

import org.jgap.*;
import org.jgap.impl.*;

/**
 * Changes the color of a polygon of each chromosome.
 *
 * @author Yann N. Dauphin
 * @since 3.4
 */
public class ColorMutationOperator
    extends MutationOperator {
  /** String containing the CVS revision. Read out via reflection!*/
  private final static String CVS_REVISION = "$Revision: 1.2 $";

  public ColorMutationOperator(final Configuration a_config,
                               final int a_desiredMutationRate)
      throws InvalidConfigurationException {
    super(a_config, a_desiredMutationRate);
  }

  @Override
  public void operate(final Population a_population,
                      final List a_candidateChromosomes) {
    if (a_population == null || a_candidateChromosomes == null) {
      // Population or candidate chromosomes list empty:
      // nothing to do.
      // -----------------------------------------------
      return;
    }
    if (getMutationRate() == 0 && getMutationRateCalc() == null) {
      // If the mutation rate is set to zero and dynamic mutation rate is
      // disabled, then we don't perform any mutation.
      // ----------------------------------------------------------------
      return;
    }
    GAConfiguration conf = (GAConfiguration) getConfiguration();
    // Determine the mutation rate. If dynamic rate is enabled, then
    // calculate it using the IUniversalRateCalculator instance.
    // Otherwise, go with the mutation rate set upon construction.
    // -------------------------------------------------------------
    boolean mutate = false;
    RandomGenerator generator = conf.getRandomGenerator();
    // It would be inefficient to create copies of each Chromosome just
    // to decide whether to mutate them. Instead, we only make a copy
    // once we've positively decided to perform a mutation.
    // ----------------------------------------------------------------
    int size = Math.min(conf.getPopulationSize(),
                        a_population.size());
    IGeneticOperatorConstraint constraint = conf.getJGAPFactory().
        getGeneticOperatorConstraint();
    for (int i = 0; i < size; i++) {
      IChromosome chrom = a_population.getChromosome(i);
      Gene[] genes = chrom.getGenes();
      IChromosome copyOfChromosome = null;
      // For each Chromosome in the population...
      // ----------------------------------------

      /* Find a polygon to mutate */
      int polygon = generator.nextInt(conf.getMaxPolygons()) *
          GAInitialChromosomeFactory.getNumberOfGenesPerPolygon();
      int target = generator.nextInt(
          GAInitialChromosomeFactory.getNumberOfColorGenesPerPolygon() - 1) +
          polygon;
      if (getMutationRateCalc() != null) {
        // If it's a dynamic mutation rate then let the calculator decide
        // whether the current gene should be mutated.
        // --------------------------------------------------------------
        mutate = getMutationRateCalc().toBePermutated(chrom, target);
      }
      else {
        // Non-dynamic, so just mutate based on the the current rate.
        // In fact we use a rate of 1/m_mutationRate.
        // ----------------------------------------------------------
        mutate = (generator.nextInt(getMutationRate()) == 0);
      }
      if (mutate) {
        // Verify that crossover allowed.
        // ------------------------------
        /**@todo move to base class, refactor*/
        if (constraint != null) {
          List v = new Vector();
          v.add(chrom);
          if (!constraint.isValid(a_population, v, this)) {
            continue;
          }
        }
        // Now that we want to actually modify the Chromosome,
        // let's make a copy of it (if we haven't already) and
        // add it to the candidate chromosomes so that it will
        // be considered for natural selection during the next
        // phase of evolution. Then we'll set the gene's value
        // to a random value as the implementation of our
        // "mutation" of the gene.
        // ---------------------------------------------------
        if (copyOfChromosome == null) {
          // ...take a copy of it...
          // -----------------------
          copyOfChromosome = (IChromosome) chrom.clone();
          // ...add it to the candidate pool...
          // ----------------------------------
          a_candidateChromosomes.add(copyOfChromosome);
          // ...then mutate all its genes...
          // -------------------------------
          genes = copyOfChromosome.getGenes();
        }
        // Process all atomic elements in the gene. For a StringGene this
        // would be as many elements as the string is long , for an
        // IntegerGene, it is always one element.
        // --------------------------------------------------------------
        if (genes[target] instanceof ICompositeGene) {
          ICompositeGene compositeGene = (ICompositeGene) genes[target];
          for (int k = 0; k < compositeGene.size(); k++) {
            mutateGene(compositeGene.geneAt(k), generator);
          }
        }
        else {
          mutateGene(genes[target], generator);
        }
      }
    }
  }

  private void mutateGene(final Gene a_gene, final RandomGenerator a_generator) {
    for (int k = 0; k < a_gene.size(); k++) {
      // Retrieve value between 0 and 1 (not included) from generator.
      // Then map this value to range -1 and 1 (-1 included, 1 not).
      // -------------------------------------------------------------
      double percentage = -1 + a_generator.nextDouble() * 2;
      // Mutate atomic element by calculated percentage.
      // -----------------------------------------------
      a_gene.applyMutation(k, percentage);
    }
  }
}
