/*
 * This file is part of JGAP.
 *
 * JGAP offers a dual license model containing the LGPL as well as the MPL.
 *
 * For licensing information please see the file license.txt included with JGAP
 * or have a look at the top of class org.jgap.Chromosome which representatively
 * includes the JGAP license policy applicable for any file delivered with JGAP.
 */
package examples.monalisa.gui;

import java.awt.*;
import java.awt.image.*;
import org.jfree.chart.*;
import org.jfree.data.xy.*;
import org.jgap.*;
import examples.monalisa.core.*;

/**
 * Class in charge of actually running the evolution process.
 *
 * @author Yann N. Dauphin
 * @since 3.4
 */
public class EvolutionRunnable
    implements Runnable {
  /** String containing the CVS revision. Read out via reflection!*/
  private final static String CVS_REVISION = "$Revision: 1.2 $";

  private GAConfiguration m_conf;

  private GeneticDrawingView m_view;

  private Genotype m_genotype;

  public EvolutionRunnable(GeneticDrawingView a_view, GAConfiguration a_conf) {
    super();
    m_conf = a_conf;
    m_view = a_view;
    m_genotype = null;
  }

  public EvolutionRunnable(GeneticDrawingView a_view, GAConfiguration a_conf,
                           Genotype a_genotype) {
    super();
    m_conf = a_conf;
    m_view = a_view;
    m_genotype = a_genotype;
  }

  public void run() {
    try {
      JFreeChart chart = m_view.getChart();
      XYSeriesCollection sc = (XYSeriesCollection) chart.getXYPlot().getDataset();
      XYSeries series = sc.getSeries(0);
      series.clear();
      if (m_genotype == null) {
        int populationSize = m_conf.getPopulationSize();
        Population pop = new Population(m_conf, populationSize);
        for (int i = 0; i < populationSize; i++) {
          pop.addChromosome(GAInitialChromosomeFactory.create(m_conf));
        }
        m_genotype = new Genotype(m_conf, pop);
      }
      //
      while (m_view.isEvolutionActivated()) {
        m_genotype.evolve();
        if (m_conf.getGenerationNr() % 25 == 0) {
          IChromosome best = m_genotype.getFittestChromosome();
          series.add(m_conf.getGenerationNr(), best.getFitnessValue());
          BufferedImage image =
              m_conf.getPhenotypeExpresser().express(best);
          Graphics g = m_view.getFittestDrawingView().getMainPanel().
              getGraphics();
          g.drawImage(image, 0, 0, m_view.getFittestDrawingView());
        }
      }
    } catch (InvalidConfigurationException e) {
      e.printStackTrace();
      System.exit( -1);
    }
  }
}
