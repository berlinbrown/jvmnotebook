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

/**
 * 
 * @author bbrown
 * 
 */
public abstract class Genome implements Comparable {

	/*
	 * Based on 
	 * http://www.c-sharpcorner.com/UploadFile/mgold/GAAdderDesign09242005053429AM/GAAdderDesign.aspx\
	 * 
	 * "Using Genetic Algorithms to Design Logic Circuits in C# By  Mike Gold February 05, 2003" 
	 * 
	 */
	
    /**
	 * 
	 */
    protected long length;

    /**
     * Cross over point.
     */
    protected int crossoverPoint;

    /**
     * Mutation Index.
     */
    protected int mutationIndex;

    /**
     * Current Fitness.
     */
    protected double currentFitness = 0.0f;

    /**
     * Intialize.
     */
    public abstract void initialize();

    /**
     * Mutate.
     */
    public abstract void mutate();

    /**
     * Uniform crossover.
     * 
     * @param g
     * @return
     */
    public abstract Genome uniformCrossover(final Genome g);

    /**
     * Cross over 2 point.
     * 
     * @param g
     * @return
     */
    public abstract Genome crossover2Point(final Genome g);

    /**
     * Cross over.
     * 
     * @param g
     * @return
     */
    public abstract Genome crossover(final Genome g);

    /**
     * Generate gene value.
     * 
     * @param min
     * @param max
     * @param position
     * @return
     */
    abstract public Object generateGeneValue(final int min, final int max,
            final int position);

    /**
     * Set cross over point.
     * 
     * @param crossoverPoint
     */
    public abstract void setCrossoverPoint(final int crossoverPoint);

    /**
     * Calculate Fitness.
     * 
     * @return
     */
    public abstract double calculateFitness();

    /**
     * Can reproduce.
     * 
     * @param fitness
     * @return
     */
    public abstract boolean canReproduce(final double fitness);

    /**
     * Can die.
     * 
     * @param fitness
     * @return
     */
    abstract public boolean canDie(final double fitness);

    /**
     * Copy gene info.
     * 
     * @param g
     */
    public abstract void copyGeneInfo(final Genome g);

} // End of Class
