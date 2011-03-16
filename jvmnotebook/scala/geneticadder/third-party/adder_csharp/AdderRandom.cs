/**
 * Using Genetic Algorithms to Design Logic Circuits in C#
 * By  Mike Gold February 05, 2003
 * The article goes to explain that GA (Genetic Algorithms) have been used by various programmers to 
 * come up with 15 previous patented inventions + (and here is the startling part) some new ones! I can see the running joke in the patent office now. 
 * 
 * http://www.c-sharpcorner.com/UploadFile/mgold/GAAdderDesign09242005053429AM/GAAdderDesign.aspx
 */

using System;
using System.Collections;
using GEPAlgorithm;

namespace GeneticAlgorithm
{
	public class AdderRandom
	{
		
		public static Random TheSeed = new Random((int) DateTime.Now.Ticks);
		
		public int Next(int m)
		{
			return TheSeed.Next(m);
		}

		public int Next(int min, int max) 
		{
			return TheSeed.Next(min, max);
		}
		
	}
	
}