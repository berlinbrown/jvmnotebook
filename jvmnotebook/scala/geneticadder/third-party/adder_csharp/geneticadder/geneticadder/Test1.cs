// Test1.cs created with MonoDevelop
// User: bbrown at 11:55 PM 4/14/2009
//
// To change standard headers go to Edit->Preferences->Coding->Standard Headers
//

using System;
using GeneticAlgorithm;

namespace GEPAlgorithm
{
	
	public class Test1
	{
		
		public static Random TheSeed = new Random((int)DateTime.Now.Ticks);
		
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{

			Console.WriteLine("Running");
			Console.WriteLine("Math-Sign Test =>" + Math.Sign(2.0  -  5.0));			
			Console.WriteLine("Next Test =>" + TheSeed.Next(10));			
			Console.WriteLine("Next Test =>" + TheSeed.Next(10, 20));
			Console.WriteLine("Done");			
			
		} // End of method

	}  // End of Class
}
