using System;
using System.Collections;
using GEPAlgorithm;


namespace GeneticAlgorithm
{
	/// <summary>
	/// Summary description for Population.
	/// 
	/// At generation 3502, here is example output.
	/// ((c|(d&b))&((c&(d&b))|a)) --> 1
	/// 
	/// </summary>
	public class Population
	{

		public const int kLength = 20;
		int nCrossover = kLength/2;
		const int kInitialPopulation = 500;
		const int kPopulationLimit = 500;
		const int kMin = 0;
		const int kMax = 8;
		const double  kMutationFrequency = 0.33f;
		const double  kDeathFitness = 0.00f;
		const double  kReproductionFitness = 0.0f;


		ArrayList Scores = new ArrayList();
		ArrayList Genomes = new ArrayList();
		ArrayList GenomeReproducers  = new ArrayList();
		ArrayList GenomeResults = new ArrayList();
		ArrayList GenomeFamily = new ArrayList();

		int		  CurrentPopulation = kInitialPopulation;
		int		  Generation = 1;
		bool	  Best2 = true;

		public Population()
		{
			//
			// TODO: Add constructor logic here
			//
			for  (int i = 0; i < kInitialPopulation; i++)
			{
				EquationGenome aGenome = new EquationGenome(kLength, kMin, kMax);
				int nCrossOver = EquationGenome.TheSeed.Next(1, (int)aGenome.Length);
				aGenome.SetCrossoverPoint(nCrossover);
				aGenome.CalculateFitness();
				Genomes.Add(aGenome);
			}

		}

		private void Mutate(Genome aGene)
		{
			if (EquationGenome.TheSeed.Next(100) < (int)(kMutationFrequency * 100.0))
			{
			  	aGene.Mutate();
			}
		}

		public void NextGeneration()
		{
			// increment the generation;
			Generation++; 


			// check who can die
			for  (int i = 0; i < Genomes.Count; i++)
			{
				if  (((Genome)Genomes[i]).CanDie(kDeathFitness))
				{
					Genomes.RemoveAt(i);
					i--;
				}
			}


			// determine who can reproduce
			GenomeReproducers.Clear();
			GenomeResults.Clear();
			for  (int i = 0; i < Genomes.Count; i++)
			{
				if (((Genome)Genomes[i]).CanReproduce(kReproductionFitness))
				{
					GenomeReproducers.Add(Genomes[i]);			
				}
			}
			
			// do the crossover of the genes and add them to the population
			DoCrossover(GenomeReproducers);

			Genomes = (ArrayList)GenomeResults.Clone();

			// mutate a few genes in the new population
			for  (int i = 0; i < Genomes.Count; i++)
			{
				Mutate((Genome)Genomes[i]);
			}

			// calculate fitness of all the genes
			for  (int i = 0; i < Genomes.Count; i++)
			{
				((Genome)Genomes[i]).CalculateFitness();
			}


//			Genomes.Sort();

			// kill all the genes above the population limit
			if (Genomes.Count > kPopulationLimit)
				Genomes.RemoveRange(kPopulationLimit, Genomes.Count - kPopulationLimit);
			
			CurrentPopulation = Genomes.Count;
			
		}

		public void CalculateFitnessForAll(ArrayList genes)
		{
			foreach(EquationGenome lg in genes)
			{
			  lg.CalculateFitness();
			}
		}

		public void DoCrossover(ArrayList genes)
		{
			ArrayList GeneMoms = new ArrayList();
			ArrayList GeneDads = new ArrayList();

			for (int i = 0; i < genes.Count; i++)
			{
				// randomly pick the moms and dad's
				if (EquationGenome.TheSeed.Next(100) % 2 > 0)
				{
					GeneMoms.Add(genes[i]);
				}
				else
				{
					GeneDads.Add(genes[i]);
				}
			}

			//  now make them equal
			if (GeneMoms.Count > GeneDads.Count)
			{
				while (GeneMoms.Count > GeneDads.Count)
				{
					GeneDads.Add(GeneMoms[GeneMoms.Count - 1]);
					GeneMoms.RemoveAt(GeneMoms.Count - 1);
				}

				if (GeneDads.Count > GeneMoms.Count)
				{
					GeneDads.RemoveAt(GeneDads.Count - 1); // make sure they are equal
				}

			}
			else
			{
				while (GeneDads.Count > GeneMoms.Count)
				{
					GeneMoms.Add(GeneDads[GeneDads.Count - 1]);
					GeneDads.RemoveAt(GeneDads.Count - 1);
				}

				if (GeneMoms.Count > GeneDads.Count)
				{
					GeneMoms.RemoveAt(GeneMoms.Count - 1); // make sure they are equal
				}
			}

			// now cross them over and add them according to fitness
			for (int i = 0; i < GeneDads.Count; i ++)
			{
				// pick best 2 from parent and children
				EquationGenome babyGene1 = null;
			    EquationGenome babyGene2 = null;

				int randomnum = EquationGenome.TheSeed.Next(100) % 3;
				if (randomnum == 0)
				{
					babyGene1 = (EquationGenome)((EquationGenome)GeneDads[i]).Crossover((EquationGenome)GeneMoms[i]);
					babyGene2 = (EquationGenome)((EquationGenome)GeneMoms[i]).Crossover((EquationGenome)GeneDads[i]);
				}
				else if (randomnum == 1)
				{
					babyGene1 = (EquationGenome)((EquationGenome)GeneDads[i]).Crossover2Point((EquationGenome)GeneMoms[i]);
					babyGene2 = (EquationGenome)((EquationGenome)GeneMoms[i]).Crossover2Point((EquationGenome)GeneDads[i]);
				}
				else
				{
					babyGene1 = (EquationGenome)((EquationGenome)GeneDads[i]).UniformCrossover((EquationGenome)GeneMoms[i]);
					babyGene2 = (EquationGenome)((EquationGenome)GeneMoms[i]).UniformCrossover((EquationGenome)GeneDads[i]);
				}
			
				GenomeFamily.Clear();
				GenomeFamily.Add(GeneDads[i]);
				GenomeFamily.Add(GeneMoms[i]);
				GenomeFamily.Add(babyGene1);
				GenomeFamily.Add(babyGene2);


				CalculateFitnessForAll(GenomeFamily);

				for (int j = 0; j < 4; j++)
				{
					CheckForUndefinedFitness((Genome)GenomeFamily[j]);
				}

				GenomeFamily.Sort();


				if (Best2 == true)
				{
					// if Best2 is true, add top fitness genes
					GenomeResults.Add(GenomeFamily[0]);					
					GenomeResults.Add(GenomeFamily[1]);					

				}
				else
				{
					GenomeResults.Add(babyGene1);
					GenomeResults.Add(babyGene2);
				}

				// if population shrunk, you can add rest of genes
				if (Population.kPopulationLimit < Genomes.Count)
				{
					GenomeResults.Add(GenomeFamily[3]);					
					GenomeResults.Add(GenomeFamily[4]);					
				}
			}

		}

		public void CheckForUndefinedFitness(Genome g)
		{
			if (double.IsNaN(g.CurrentFitness))
				g.CurrentFitness = 0.01f;
		}


		public void WriteNextGeneration()
		{			
			// just write the top 20
			Console.WriteLine("Generation {0}\n", Generation);
			for  (int i = 0; i <  CurrentPopulation ; i++)
			{
				Console.WriteLine("Population Size=" + CurrentPopulation + " i=" + i  + ((Genome)Genomes[i]).ToString());
			}

			Console.WriteLine("Generation #{0}, Hit the enter key to continue...\n", Generation);
			//Console.ReadLine();
		}

		public bool Converged()
		{
			int histogram = 0;
			for (int i=1; i < Scores.Count; i++)
			{
				if ((int)(((double)Scores[i])*10000) == (int)(((double)Scores[i-1])*10000))
				{
					histogram++;
				}
				else
				{
				  histogram = 0;
				}
			}

			if (histogram > 5)
				return true;

			return false;
		}

		public void WriteNextGenerationBest()
		{
			Genomes.Sort();
			if (Genomes.Count > 0)
			{
				Console.WriteLine(((Genome)Genomes[0]).ToString());
				Scores.Add(((EquationGenome)Genomes[0]).CurrentFitness);  // add top score;
			}
		}

		public Genome GetNextGenerationBest()
		{
			Genomes.Sort();
			return (Genome) Genomes[0];			
		}
		
	}
}
