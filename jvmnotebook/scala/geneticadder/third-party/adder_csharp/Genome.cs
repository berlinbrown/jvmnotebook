using System;
using System.Collections;

namespace GeneticAlgorithm
{
	/// <summary>
	/// Summary description for Genome.
	/// </summary>
	public abstract class Genome : IComparable
	{
		public long Length;
		public int  CrossoverPoint;
		public int  MutationIndex;
		public double CurrentFitness = 0.0f;

		abstract public void     Initialize();
		abstract public void     Mutate();
		abstract public Genome   UniformCrossover(Genome g);
		abstract public Genome   Crossover2Point(Genome g);
		abstract public Genome   Crossover(Genome g);
		abstract public object   GenerateGeneValue(object min, object max, int position);
		abstract public void     SetCrossoverPoint(int crossoverPoint);
		abstract public double   CalculateFitness();
		abstract public bool     CanReproduce(double fitness);
		abstract public bool     CanDie(double fitness);
		abstract public string   ToString();
		abstract public void	 CopyGeneInfo(Genome g);

		
		abstract public int CompareTo(object a);

	}
}
