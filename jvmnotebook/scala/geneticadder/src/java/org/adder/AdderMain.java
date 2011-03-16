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

public class AdderMain {

	/*
	 * Based on 
	 * http://www.c-sharpcorner.com/UploadFile/mgold/GAAdderDesign09242005053429AM/GAAdderDesign.aspx\
	 * 
	 * "Using Genetic Algorithms to Design Logic Circuits in C# By  Mike Gold February 05, 2003"  
	 */
	
	public static final void main(final String [] args) {
		
		System.out.println("Running");
		
		final Population p = new Population();
		p.writeNextGeneration();

		int count = 0;
		while (p.converged() == false) {
			
			System.out.println("######################");
			System.out.println("######################");
			System.out.println("######################");
			System.out.println("######################");
			System.out.println("######################");
			System.out.println("###################### -------------------- " + count);
			
			p.nextGeneration();
			if ((count % 50) == 0) {
				p.writeNextGeneration();
			}
			
			if (count > 50) {
				break;
			}
			
			count++;
		}
		
		System.out.println("Done");
		
	}
		
}
