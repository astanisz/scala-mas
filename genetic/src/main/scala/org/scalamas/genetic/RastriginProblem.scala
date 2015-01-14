package org.scalamas.genetic

import org.scalamas.mas.AgentRuntimeComponent
import org.scalamas.mas.random.RandomGenerator

import scala.math._

/**
 * An implementation of genetic operators for finding the minimum of the Rastrigin function.
 */
trait RastriginProblem extends GeneticProblem {
  this: AgentRuntimeComponent with RandomGenerator =>

  type Genetic = RastriginOps

  def genetic = new RastriginOps

  class RastriginOps extends GeneticOps[RastriginOps] {

    type Feature = Double
    type Solution = Array[Feature]
    type Evaluation = Double

    def config = agentRuntime.config.getConfig("genetic.rastrigin")
    val problemSize = config.getInt("problemSize")
    val mutationChance = config.getDouble("mutationChance")
    val mutationRate = config.getDouble("mutationRate")

    def generate = Array.fill(problemSize)(-50 + random * 100)

    def evaluate(solution: Solution) = {
      solution.foldLeft(0.0)(
        (sum, x) => sum + 10 + x * x - 10 * cos(2 * Pi * x))
    }

    // TODO take problemSize into account
    val minimal = 10000.0

    val ordering = Ordering[Double].reverse

    def transform(solution: Solution) =
      mutateSolution(solution)

    def transform(solution1: Solution, solution2: Solution) =
      mutateSolutions(recombineSolutions(solution1, solution2))

    def mutateSolution(s: Solution) =
      if (random < mutationChance)
        s.map(f => if (random < mutationRate) mutateFeature(f) else f)
      else
        s

    def mutateSolutions(s: (Solution, Solution)) =
      (mutateSolution(s._1), mutateSolution(s._2))

    def mutateFeature(f: Feature): Feature = {
      val range = random match {
        case x if x < 0.2 => 5.0
        case x if x < 0.4 => 0.2
        case _ => 1.0
      }
      f + range * tan(Pi * (random - 0.5))
    }

    def recombineSolutions(s1: Solution, s2: Solution): (Solution, Solution) = {
      val (s3, s4) = s1.zip(s2).map(recombineFeatures).unzip
      (s3.toArray, s4.toArray)
    }

    def recombineFeatures(features: (Feature, Feature)): (Feature, Feature) = {
      val a = min(features._1, features._2)
      val b = max(features._1, features._2)
      (a + (b - a) * random, a + (b - a) * random)
    }
  }
}
