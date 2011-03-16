/********************************************************************
 *
 * Copyright (c) 2006-2007 Berlin Brown and botnode.com  All Rights Reserved
 *
 * http://www.opensource.org/licenses/bsd-license.php

 * All rights reserved.

 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:

 * * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * * Neither the name of the Botnode.com (Berlin Brown) nor
 * the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Date:
 * Main Description:
 * Contact: Berlin Brown <berlin dot brown at gmail.com>
 *********************************************************************/
package org.adder;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Using Genetic Algorithms to Design Logic Circuits in Java.
 * 
 * @author bbrown
 * 
 */
public class EquationGenome extends Genome {

    /*
     * Based on
     * http://www.c-sharpcorner.com/UploadFile/mgold/GAAdderDesign09242005053429AM
     * /GAAdderDesign.aspx\
     * 
     * "Using Genetic Algorithms to Design Logic Circuits in C# By  Mike Gold February 05, 2003"
     */

    /**
     * Number of Symbols
     */
    public static final int NumSymbols = 4;

    /**
     * Number of Functions
     */
    public static final int NumFunctions = 4;

    /**
     * The Random Seem.
     */
    public static final Random TheSeed = new Random(System.currentTimeMillis());

    /**
     * Calculation Array as a String.
     */
    public static final String[] CalculationStringArray = new String[Population.kLength];

    /**
     * Calculation Array.
     */
    public static final byte[] CalculationArray = new byte[Population.kLength];

    /**
     * The min.
     */
    private int theMin = 0;

    /**
     * The max.
     */
    private int theMax = 6;

    public boolean trialFitness;
    private List<Gene> theArray = new ArrayList<Gene>();

    final byte measure[][] = new byte[][] { { 0, 0, 0, 0, 0, 0, 0, 0 }, // ROW 0
            { 0, 0, 0, 1, 0, 0, 0, 1 }, // ROW 1
            { 0, 0, 1, 0, 0, 0, 1, 0 }, // ROW 2
            { 0, 0, 1, 1, 0, 0, 1, 1 }, // ROW 3
            { 0, 1, 0, 0, 0, 0, 0, 1 }, // ROW 4
            { 0, 1, 0, 1, 0, 0, 1, 0 }, // ROW 5
            { 0, 1, 1, 0, 0, 0, 1, 1 }, // ROW 6
            { 0, 1, 1, 1, 0, 1, 0, 0 }, // ROW 7
            { 1, 0, 0, 0, 0, 0, 1, 0 }, // ROW 8
            { 1, 0, 0, 1, 0, 0, 1, 1 }, // ROW 9
            { 1, 0, 1, 0, 0, 1, 0, 0 }, // ROW 10
            { 1, 0, 1, 1, 0, 1, 0, 1 }, // ROW 11
            { 1, 1, 0, 0, 0, 0, 1, 1 }, // ROW 12
            { 1, 1, 0, 1, 0, 1, 0, 0 }, // ROW 13
            { 1, 1, 1, 0, 0, 1, 0, 1 }, // ROW 14
            { 1, 1, 1, 1, 0, 1, 1, 0 } // ROW 15
    };

    public static class Gene {

        /**
		 * 
		 */
        public int instruction1;

        /**
		 * 
		 */
        public int instruction2;

        /**
		 * 
		 */
        public int operation;

    };

    /**
     * Compare to.
     */
    public int compareTo(final Object a) {

        final EquationGenome gene1 = this;
        final EquationGenome gene2 = (EquationGenome) a;

        final Double gened1 = new Double(gene1.currentFitness);
        final Double gened2 = new Double(gene2.currentFitness);
        return gened1.compareTo(gened2);
    }

    /**
     * Set the cross over point.
     */
    public void setCrossoverPoint(int crossoverPoint) {

        this.crossoverPoint = crossoverPoint;

    }

    /**
     * Genome constructor.
     */
    public EquationGenome() {
        ;

    }

    /**
     * Genome constructor.
     * 
     * @param length
     * @param min
     * @param max
     */
    public EquationGenome(long length, final int min, final int max) {

        this.length = length;
        theMin = (int) min;
        theMax = (int) max;

        for (int i = 0; i < length; i++) {

            final Gene nextValue = (Gene) generateGeneValue(min, max, i);
            theArray.add(nextValue);

        }
    }

    public void initialize() {
        ;
    }

    public boolean canDie(double fitness) {

        if (currentFitness <= (fitness * 100.0f)) {
            return true;
        }

        return false;
    }

    public boolean canReproduce(double fitness) {

        final double nint = (double) EquationGenome.TheSeed.nextInt(100);
        if (nint >= fitness * 100.0f) {
            return true;
        }

        return false;
    }

    public static final int nextInt(final Random r, final int min, final int max) {

        final int diff = max - min;
        final int n = r.nextInt(diff);
        return n + min;

    }

    public Object generateGeneValue(final int min, final int max, final int position) {

        final Gene g = new Gene();
        int nextSeed = 0;
        nextSeed = nextInt(TheSeed, (int) min, (int) max);
        g.operation = nextSeed;

        if (position == 0) {

            // Generate 0 or 1 for a
            g.operation = nextInt(TheSeed, 0, NumSymbols);
            return g;
        }

        if (nextSeed > 1) {

            nextSeed = nextInt(TheSeed, (int) min, position);
            g.instruction1 = nextSeed;
            nextSeed = nextInt(TheSeed, (int) min, position);
            g.instruction2 = nextSeed;
        }

        return g;
    }

    public void mutate() {

        int AffectedGenes = TheSeed.nextInt((int) 3);
        for (int i = 0; i < AffectedGenes; i++) {

            mutationIndex = nextInt(TheSeed, 0, (int) length);
            final Gene gene = (Gene) this.generateGeneValue(theMin, theMax, mutationIndex);
            theArray.set(mutationIndex, gene);
        }

    }

    public String getOperationString(final int operation) {

        String result = "";
        switch (operation) {
        case 4: // *
            result = "&";
            break;
        case 5: // /
            result = "|";
            break;
        case 6: // +
            result = "^";
            break;
        case 7: // -
            result = "~";
            break;
        default:
            // +
            break;
        } // end switch
        
		System.out.println("@@ GetOperationString =>" + result + " " + operation);
        
        return result;

    }

    public byte doOperation(final byte a, final byte b, final int operation) {

        byte result = 0;
        switch (operation) {

        case 4: // and
            result = (byte) (a & b & 1);
            break;
        case 5: // or
            result = (byte) ((a | b) & 1);
            break;
        case 6: // xor
            result = (byte) ((a ^ b) & 1);
            break;
        case 7: // -
            result = (byte) ((~a) & 1);
            break;
        default:
            // +
            break;
        }; // end switch

		//System.out.println("@@ DoOperation =>" + result + " a=" + a + " b=" + b + " c=" + operation);        
        return result;

    }

    public String formStepsString() {

        String _result = "\n";
        int count = 0;
        for (Gene g : theArray) {

            if (g.operation < NumSymbols) {

                // a or b, assign value
                if (g.operation == 0) {
                    _result += count + ": " + "a\n";
                } else {
                    _result += count + ": " + "b\n";
                }

            } else if (g.operation == 8) {

                _result += count + ": " + "PI\n";

            } else {

                // operation, use it to fill calculation in array
                _result += count + ": " + getOperationString(g.operation) + " " + g.instruction1 + ", " + g.instruction2 + "\n";
            }
            count++;
        }

        _result += "\n\n";
        return _result;
    }

    public String formEquationString() {

        String _result = "";
        int count = 0;
        for (Gene g : theArray) {

            if (g.operation < NumSymbols) {

                // a or b, assign value
                switch (g.operation) {
                case 0:
                    CalculationStringArray[count] = "a";
                    break;
                case 1:
                    CalculationStringArray[count] = "b";
                    break;
                case 2:
                    CalculationStringArray[count] = "c";
                    break;
                case 3:
                    CalculationStringArray[count] = "d";
                    break;
                }

            } else if (g.operation == 8) {

                CalculationStringArray[count] = "1";

            } else if (g.operation == 7) {

                CalculationStringArray[count] = "("
                        + CalculationStringArray[g.instruction1]
                        + getOperationString(g.operation) + ")";

            } else if (g.operation == 9) { // unary

                CalculationStringArray[count] = "("
                        + getOperationString(g.operation)
                        + CalculationStringArray[g.instruction1] + ")" + ")";

            } else {

                // operation, use it to fill calculation in array
                CalculationStringArray[count] = "("
                        + CalculationStringArray[g.instruction1]
                        + getOperationString(g.operation)
                        + CalculationStringArray[g.instruction2] + ")";
            }
            count++;
        }

        _result = CalculationStringArray[Population.kLength - 1];
        return _result;
    }

    public byte performCalculation(byte a, byte b, byte c, byte d) {

        int count = 0;
        for (Gene g : theArray) {

            if (g.operation < NumSymbols) {

                // a or b, assign value
                switch (g.operation) {
                case 0:
                    CalculationArray[count] = a;
                    break;
                case 1:
                    CalculationArray[count] = b;
                    break;
                case 2:
                    CalculationArray[count] = c;
                    break;
                case 3:
                    CalculationArray[count] = d;
                    break;
                }
                ;

            } else {
                // operation, use it to fill calculation in array
                CalculationArray[count] = doOperation(CalculationArray[g.instruction1], CalculationArray[g.instruction2], g.operation);
            } // End of if - else

            //System.out.println(" $$ Count =>" + count + " Array => " + CalculationArray[count]);             
            count++;
        } // End of For Each

        return CalculationArray[theArray.size() - 1]; // return last calculation
    }

    public double calculateFullAdder() {

        // use the node to calculate the fitness
        double calc = 0.0f;
        double sum = 0.0f;
        int count = measure[0].length;

        for (int i = 0; i < count; i++) {

            calc = performCalculation(measure[i][0], measure[i][1], measure[i][2], measure[i][3]);

			//System.out.println("###-->" + measure[i][measure[1].length - 1]);
			//System.out.println("### (2) -->" + Math.abs(measure[i][measure[1].length - 1] - calc));			
			//System.out.println("### (3) -->" + (measure[i][measure[1].length - 1] - calc));
			            
            // /////////////////////////////////////////////
            // Uncomment the following lines for using the error handling for
            // different output bits
            // /////////////////////////////////////////////
            // bit 0 fitness
            // For this fitness/bit we should see the following (d^b)
            double error = 100.0f - Math.abs(measure[i][measure[1].length - 1] - calc);
                                    
            // bit 1 fitness
            // For this fitness/bit we should see the following: (((b&d)^a)^c)
            // --> 1
            // double error = 100.0f - Math.abs(measure[i][measure[1].length -
            // 2] - calc);
            // bit 2 fitness
            // For this fitness/bit we should see the following:
            // ((c|(d&b))&(a|c(&(d&b)))) -> 1
            // double error = 100.0f - Math.abs(measure[i][measure[1].length -
            // 3] - calc);
            
            //System.out.println("### (4) -->  error => " + error);
            //System.out.println("### (5) -->  sum=" + sum);
            
            sum += error;
        }        
        //System.out.println("### (6) --> sum=" + sum);
        
        this.currentFitness = sum / (measure[0].length * 100.0f);
        //System.out.println("   (7) fitness=>" + this.currentFitness); 
                
        
        if (Double.isNaN(this.currentFitness)) {
            this.currentFitness = 0.01f;
        }

        return this.currentFitness;
    }

    public double calculateFitness() {

        currentFitness = calculateFullAdder();
        
        System.out.println("________________________" + currentFitness);
        
        if (currentFitness < 0.0f) {
            currentFitness = 0.01f;
        }
        return currentFitness;
    }

    public String toString() {

        String strResult = "";
        strResult += " --> " + this.formEquationString();
        strResult += " --> " + currentFitness;

        return strResult;
    }

    public void copyGeneInfo(final Genome dest) {

        EquationGenome theGene = (EquationGenome) dest;
        theGene.length = length;
        theGene.theMin = theMin;
        theGene.theMax = theMax;
    }

    public Genome uniformCrossover(final Genome g) {

        EquationGenome aGene1 = new EquationGenome();
        EquationGenome aGene2 = new EquationGenome();
        g.copyGeneInfo(aGene1);
        g.copyGeneInfo(aGene2);

        // swap genes randomly
        EquationGenome crossingGene = (EquationGenome) g;
        for (int i = 0; i < length; i++) {

            if ((TheSeed.nextInt(100) % 2) == 0) {
                aGene1.theArray.add(crossingGene.theArray.get(i));
                aGene2.theArray.add(theArray.get(i));
            } else {
                aGene1.theArray.add(theArray.get(i));
                aGene2.theArray.add(crossingGene.theArray.get(i));
            }

        }
        // 50/50 chance of returning gene1 or gene2
        EquationGenome aGene = null;
        if (TheSeed.nextInt(2) == 1) {
            aGene = aGene1;
        } else {
            aGene = aGene2;
        }
        return aGene;
    }

    public Genome crossover2Point(final Genome g) {

        final EquationGenome aGene1 = new EquationGenome();
        final EquationGenome aGene2 = new EquationGenome();
        g.copyGeneInfo(aGene1);
        g.copyGeneInfo(aGene2);

        // Pick a random crossover point
        int CrossoverPoint1 = nextInt(TheSeed, 1, (int) length);
        int CrossoverPoint2 = nextInt(TheSeed, CrossoverPoint1, (int) length);
        // normalize
        if (CrossoverPoint1 > CrossoverPoint2) {
            int temp = CrossoverPoint1;
            CrossoverPoint1 = CrossoverPoint2;
            CrossoverPoint2 = temp;
        }

        EquationGenome crossingGene = (EquationGenome) g;

        for (int j = 0; j < CrossoverPoint1; j++) {
            aGene1.theArray.add(theArray.get(j));
            aGene2.theArray.add(crossingGene.theArray.get(j));
        }

        for (int j = CrossoverPoint1; j < CrossoverPoint2; j++) {
            if (j >= 0) {
                aGene1.theArray.add(crossingGene.theArray.get(j));
                aGene2.theArray.add(theArray.get(j));
            } else {
                throw new RuntimeException(
                        "<crossover2Point> j/crossoverpoint1 less than zero = "
                                + j);
            }
        }

        for (int j = CrossoverPoint2; j < length; j++) {

            aGene1.theArray.add(theArray.get(j));
            aGene2.theArray.add(crossingGene.theArray.get(j));
        }

        // 50/50 chance of returning gene1 or gene2
        EquationGenome aGene = null;
        if (TheSeed.nextInt(2) == 1) {
            aGene = aGene1;
        } else {
            aGene = aGene2;
        }

        return aGene;
    }

    public Genome crossover(final Genome g) {

        EquationGenome aGene1 = new EquationGenome();
        EquationGenome aGene2 = new EquationGenome();
        g.copyGeneInfo(aGene1);
        g.copyGeneInfo(aGene2);

        // Pick a random crossover point
        crossoverPoint = nextInt(TheSeed, 1, (int) length);
        EquationGenome crossingGene = (EquationGenome) g;

        for (int i = 0; i < crossoverPoint; i++) {

            aGene1.theArray.add(crossingGene.theArray.get(i));
            aGene2.theArray.add(theArray.get(i));

        }
        for (int j = crossoverPoint; j < length; j++) {
            if (j >= 0) {
                aGene1.theArray.add(theArray.get(j));
                aGene2.theArray.add(crossingGene.theArray.get(j));
            } else {
                throw new RuntimeException(
                        "WARN: <crossover> j/crossoverpoint less than zero = "
                                + j);
            }
        }

        // 50/50 chance of returning gene1 or gene2
        EquationGenome aGene = null;
        if (TheSeed.nextInt(2) == 1) {
            aGene = aGene1;
        } else {
            aGene = aGene2;
        }
        return aGene;
    } // End of the Method Cross over

} // End of the Class
