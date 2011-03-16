/**
 * Based on 
 * http://www.c-sharpcorner.com/UploadFile/mgold/GAAdderDesign09242005053429AM/GAAdderDesign.aspx\
 * 
 * "Using Genetic Algorithms to Design Logic Circuits in C# By  Mike Gold February 05, 2003" 
 * 
 */

using System;
using GeneticAlgorithm;

namespace GEPAlgorithm
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	class Class1
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>	
		[STAThread]
		static void Main(string[] args)
		{
			Population p = new Population();
			p.WriteNextGeneration();

			int count = 0;
			byte res = 0;
			EquationGenome resGenome;
			while (p.Converged() == false)
			{
				p.NextGeneration();
				if (count % 50 == 0) {
					p.WriteNextGeneration();
										
					resGenome = (EquationGenome) p.GetNextGenerationBest(); 
					res = resGenome.PerformCalculation(0, 1, 1, 1);			
					Console.WriteLine("<Loop> Result => {0}", res.ToString());
					
				}
				count++;			
			
				if (count > 40000) {
				    Console.WriteLine("---------------");
				    p.WriteNextGenerationBest();
				    Console.WriteLine("---------------");
				    break;
				} // End of if
			} // End of the while
			
			Console.WriteLine("Done");

			resGenome = (EquationGenome) p.GetNextGenerationBest(); 
			res = resGenome.PerformCalculation(0, 1, 1, 1);			
			Console.WriteLine("Result => {0}", res.ToString());
			
	   } // End of Method
		
	} // End of Class
	
} // End of Namespace
