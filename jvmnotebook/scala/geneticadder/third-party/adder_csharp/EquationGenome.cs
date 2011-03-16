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
    /// <summary>
    /// Summary description for EquationNode.
    /// Equation node represents a symbolic equation that solves a problem
    /// In this case we'll use the following keys to represent our node
    /// 0: a 1: b 2: *  3: / 4: + 5: -  6: %  7: S (sin) 8: PI 9: Power
    /// </summary>
    /// 
    public class EquationGenome : Genome
    {
        public const int NumSymbols = 4;
        public const int NumFunctions = 4;

        /**
         * x, y, z is the desired binary output or the sum of a b  + c d.  
         * For example  01 + 1 1  = 0 1 0 0  or in base 10, 1 + 3 = 4
         * 
         * Measure consists of a 16 row * 8 col array
         */
        byte[,] measure = new byte[,]{    // a  b  c  d
                                {0, 0, 0, 0, 0, 0, 0, 0},  
                                {0, 0, 0, 1, 0, 0, 0, 1},  
                                {0, 0, 1, 0, 0, 0, 1, 0},  
                                {0, 0, 1, 1, 0, 0, 1, 1},  
                                {0, 1, 0, 0, 0, 0, 0, 1},  
                                {0, 1, 0, 1, 0, 0, 1, 0},  
                                {0, 1, 1, 0, 0, 0, 1, 1},    
                                {0, 1, 1, 1, 0, 1, 0, 0},    
                                {1, 0, 0, 0, 0, 0, 1, 0},    
                                {1, 0, 0, 1, 0, 0, 1, 1},    
                                {1, 0, 1, 0, 0, 1, 0, 0},      
                                {1, 0, 1, 1, 0, 1, 0, 1},    
                                {1, 1, 0, 0, 0, 0, 1, 1},      
                                {1, 1, 0, 1, 0, 1, 0, 0},      
                                {1, 1, 1, 0, 0, 1, 0, 1},      
                                {1, 1, 1, 1, 0, 1, 1, 0},      
        };
                                    
        
        public struct Gene
        {
            public int instruction1;
            public int instruction2;
            public int operation;
        };

        ArrayList TheArray = new ArrayList();
        public static Random TheSeed = new Random((int)DateTime.Now.Ticks);
        
        int TheMin = 0;
        int TheMax = 6;
        int CurrentXPos = 0;
        int CurrentYPos = 0;
        int PreviousSeed = 2;
        public bool TrialFitness; // this needs to be perfect or else we have to throw out the gene

        public override int CompareTo(object a)
        {
            EquationGenome Gene1 = this;
            EquationGenome Gene2 = (EquationGenome)a;
            return Math.Sign(Gene2.CurrentFitness  -  Gene1.CurrentFitness);
        }


        public override void SetCrossoverPoint(int crossoverPoint)
        {
            CrossoverPoint =    crossoverPoint;
        }

        public EquationGenome()
        {
        }


        public EquationGenome(long length, object min, object max)
        {

            Length = length;
            TheMin = (int)min;
            TheMax = (int)max;
            CurrentXPos = 0;
            CurrentYPos = 0;

            for (int i = 0; i < Length; i++)
            {
                Gene nextValue = (Gene)GenerateGeneValue(min, max, i);
                TheArray.Add(nextValue);
            }
        }

        public override void Initialize()
        {

        }

        public override bool CanDie(double fitness)
        {
            if (CurrentFitness <= (int)(fitness * 100.0f))
            {
                return true;
            }

            return false;
        }


        public override bool CanReproduce(double fitness)
        {
            if (EquationGenome.TheSeed.Next(100) >= (int)(fitness * 100.0f))
            {
                return true;
            }

            return false;
        }


        public override object GenerateGeneValue(object min, object max, int position)
        {
            Gene g = new Gene();
            int nextSeed = 0;
            nextSeed = TheSeed.Next((int)min, (int)max);
            g.operation = nextSeed;

            if (position == 0) // special case, want to generate a symbol for first one
            {
               g.operation = TheSeed.Next(0, NumSymbols);  // generate 0 or 1, for a and b
               return g;
            }

            if (nextSeed > 1) // we have an operation, need postion
            {
                nextSeed = TheSeed.Next((int)min, position);
                g.instruction1 = nextSeed;
                nextSeed = TheSeed.Next((int)min, position);
                g.instruction2 = nextSeed;
            }
            
            return g;
        }

    
        public override void Mutate()
        {
            int AffectedGenes = TheSeed.Next((int)3); // determine how many genes to mutate
            for (int i = 0; i < AffectedGenes; i++)
            {
                MutationIndex = TheSeed.Next(0, (int)Length);
                //              int val = (int)GenerateGeneValue(TheMin, TheMax);
                TheArray[MutationIndex] = this.GenerateGeneValue(TheMin, TheMax, MutationIndex);
            }

        }


        // This fitness function calculates the fitness of distance travelled
        // from upper left to lower right


        public string GetOperationString(int operation)
        {
            string result = "";
            switch(operation)
            {
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

            return result;

        }


        public byte DoOperation(byte a, byte b, int operation)
        {
            byte result = 0;
            switch(operation)
            {
                case 4: // and
                    result = (byte)(a & b & 1);
                    break;
                case 5: // or 
                    result = (byte) ((a | b) & 1);
                    break;
                case 6: // xor
                    result =  (byte)((a ^ b) & 1);
                    break;
                case 7: // - 
                    result = (byte)((~a) & 1);
                    break;
                default:
                    // +
                    break;
            } // end switch

            return result;

        }

        string[] CalculationStringArray = new string[Population.kLength];

        public string FormStepsString()
        {
            string _result = "\n";

            int count = 0;
            foreach (Gene g in TheArray)
            {
                if (g.operation < NumSymbols)
                {
                    // a or b, assign value
                    if (g.operation == 0) {
                        _result += " [oper=" + g.operation + "]" + count.ToString() + "  : " + "a\n";
                    } else {
                        _result += " [oper=" + g.operation + "]" + count.ToString() +"  : " + "b\n";
                    } // End of the if - else
                }
                else if (g.operation == 8)
                {
                    _result += count.ToString() +": " + "PI\n";
                }
                else
                {
                    // operation, use it to fill calculation in array
                    _result += count.ToString() +": " + GetOperationString(g.operation) + " " + g.instruction1.ToString() + ", " + g.instruction2.ToString() + "\n";
                } // End of the if - else

                count++;
            }

            _result += "\n\n";
            return _result;
        }


        public string FormEquationString()
        {
            string _result = "";

            int count = 0;
            foreach (Gene g in TheArray)
            {
                if (g.operation < NumSymbols)
                {
                    // a or b, assign value

                    switch (g.operation)
                    {
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

                }
                else if (g.operation == 8)
                {
                    CalculationStringArray[count] = "1";
                }
                else if (g.operation == 7) // unary
                {
                    CalculationStringArray[count] =   "(" + CalculationStringArray[g.instruction1] + GetOperationString(g.operation) + ")";
                }
                else if (g.operation == 9) // unary
                {
                    CalculationStringArray[count] =   "(" + GetOperationString(g.operation) + CalculationStringArray[g.instruction1]  +  ")" + ")";
                }
                else
                {
                    // operation, use it to fill calculation in array
                    CalculationStringArray[count] = "(" + CalculationStringArray[g.instruction1] + GetOperationString(g.operation) + CalculationStringArray[g.instruction2] + ")";
                }

                count++;
            }

            _result = CalculationStringArray[Population.kLength - 1];
            return _result;
        }

        // Calculation Array == 20
        byte[] CalculationArray = new byte[Population.kLength];
        
        public byte PerformCalculation(byte a, byte b, byte c, byte d)
        {
            int count = 0;

            // NumSymbols == 4 | 0, 1, 2, 3         
        foreach (Gene g in TheArray)
            {
            if (g.operation < NumSymbols)
            {
                // a or b, assign value
                switch (g.operation)
                {
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
            }
            else
            {
                // operation, use it to fill calculation in array
                CalculationArray[count] = DoOperation(CalculationArray[g.instruction1], CalculationArray[g.instruction2], g.operation);
            } // End of the if - else 

                count++;
            } // End of the For Each
            
            //for (int i = 0; i < CalculationArray.Length; i++) {
            //  Console.WriteLine("   #*v2 calc=>" + CalculationArray[i] + "    i=" + i + " 0=" + a + " 1=" + b + " 2=" +  c + " 3=" + d);              
            //} // End of the for
            
            // return last calculation
            return CalculationArray[TheArray.Count - 1];  
        }
    
        public double CalculateFullAdder()
        {
            int index = 0;
            // use the node to calculate the fitness

            double calc = 0.0f;
            double sum = 0.0f;
            int count = measure.GetLength(0);
            
            // The rows count should equal == 16
            // Console.WriteLine("\n\n***** COUNT/0 rows " + count);
            for (int i = 0; i < count; i++)
            {
                // Why is the calc return binary output.
                calc = PerformCalculation(measure[i, 0],  measure[i, 1], measure[i,2], measure[i,3]);

                ////////////////////////////////////////////
                // By uncommenting the line that checks bit 1 fitness, we can run our genetic algorithm on our adder inputs against the bit 1 output.  
                // Below is the fitness function determining the error of the output calculated for the genome against the desired output of bit 1
                ////////////////////////////////////////////
                // bit 0 fitness
                //double error = 100 - Math.Abs(measure[i,measure.GetLength(1) - 1] - calc); // last byte
                // bit 1 fitness
                //double error = 100 - Math.Abs(measure[i,measure.GetLength(1) - 2] - calc); // last byte
                // bit 2 fitness                
                double error = 100 - Math.Abs(measure[i,measure.GetLength(1) - 3] - calc); // last byte

                sum +=  error;
            }
            //Console.WriteLine("   sum=>" + sum);
            CurrentFitness = sum/(measure.GetLength(0) * 100);
                                
            // The fitness value will approach 1
            //Console.WriteLine("   fitness=>" + CurrentFitness);
            if (double.IsNaN(CurrentFitness)) {
                CurrentFitness = 0.01f;
            }

            return CurrentFitness;
        }

        public override double CalculateFitness()
        {
            CurrentFitness = CalculateFullAdder();
            if (CurrentFitness < 0.0f) {
                CurrentFitness = 0.01f;
            }
            return CurrentFitness;
        }


        public override string ToString()
        {
            string strResult = "";
            int index = 0;
            
            strResult += " -->  " + this.FormEquationString();
            strResult += " --> " + CurrentFitness.ToString();

            return strResult;
        }

        public override void CopyGeneInfo(Genome dest)
        {
            EquationGenome theGene = (EquationGenome)dest;
            theGene.Length = Length;
            theGene.TheMin = TheMin;
            theGene.TheMax = TheMax;
        }

        public override Genome UniformCrossover(Genome g)
        {
            EquationGenome aGene1 = new EquationGenome();
            EquationGenome aGene2 = new EquationGenome();
            g.CopyGeneInfo(aGene1);
            g.CopyGeneInfo(aGene2);

            // swap genes randomly
            EquationGenome CrossingGene = (EquationGenome)g;
            for (int i = 0; i < Length; i++)
            {
                if (TheSeed.Next(100) % 2 == 0)
                {
                    aGene1.TheArray.Add(CrossingGene.TheArray[i]);
                    aGene2.TheArray.Add(TheArray[i]);
                }
                else
                {
                    aGene1.TheArray.Add(TheArray[i]);
                    aGene2.TheArray.Add(CrossingGene.TheArray[i]);
                }

            }


            // 50/50 chance of returning gene1 or gene2
            EquationGenome aGene = null;
            if (TheSeed.Next(2) == 1)           
            {
                aGene = aGene1;
            }
            else
            {
                aGene = aGene2;
            }

            return aGene;
        }


        public override Genome Crossover2Point(Genome g)
        {
            EquationGenome aGene1 = new EquationGenome();
            EquationGenome aGene2 = new EquationGenome();
            g.CopyGeneInfo(aGene1);
            g.CopyGeneInfo(aGene2);

            // Pick a random crossover point
            int CrossoverPoint1 = TheSeed.Next(1, (int)Length);
            int CrossoverPoint2 = TheSeed.Next(CrossoverPoint1, (int)Length);
            // normalize
            if (CrossoverPoint1 > CrossoverPoint2)
            {
                int temp = CrossoverPoint1;
                CrossoverPoint1 = CrossoverPoint2;
                CrossoverPoint2 = temp;
            }

            EquationGenome CrossingGene = (EquationGenome)g;

            for (int j = 0; j < CrossoverPoint1; j++)
            {
                aGene1.TheArray.Add(TheArray[j]);
                aGene2.TheArray.Add(CrossingGene.TheArray[j]);
            }

            for (int j = CrossoverPoint1; j < CrossoverPoint2; j++)
            {
                aGene1.TheArray.Add(CrossingGene.TheArray[j]);
                aGene2.TheArray.Add(TheArray[j]);
            }


            for (int j = CrossoverPoint2; j < Length; j++)
            {
                aGene1.TheArray.Add(TheArray[j]);
                aGene2.TheArray.Add(CrossingGene.TheArray[j]);
            }


            // 50/50 chance of returning gene1 or gene2
            EquationGenome aGene = null;
            if (TheSeed.Next(2) == 1)           
            {
                aGene = aGene1;
            }
            else
            {
                aGene = aGene2;
            }

            return aGene;
        }


        public override Genome Crossover(Genome g)
        {
            EquationGenome aGene1 = new EquationGenome();
            EquationGenome aGene2 = new EquationGenome();
            g.CopyGeneInfo(aGene1);
            g.CopyGeneInfo(aGene2);

            // Pick a random crossover point
            CrossoverPoint = TheSeed.Next(1, (int)Length);

            EquationGenome CrossingGene = (EquationGenome)g;
            for (int i = 0; i < CrossoverPoint; i++)
            {
                aGene1.TheArray.Add(CrossingGene.TheArray[i]);
                aGene2.TheArray.Add(TheArray[i]);
            }

            for (int j = CrossoverPoint; j < Length; j++)
            {
                aGene1.TheArray.Add(TheArray[j]);
                aGene2.TheArray.Add(CrossingGene.TheArray[j]);
            }

            // 50/50 chance of returning gene1 or gene2
            EquationGenome aGene = null;
            if (TheSeed.Next(2) == 1)           
            {
                aGene = aGene1;
            }
            else
            {
                aGene = aGene2;
            }

            return aGene;
        }

        public int this[int arrayindex]
        {
            get
            {
                return (int)TheArray[arrayindex];
            }
        }

    }
}
