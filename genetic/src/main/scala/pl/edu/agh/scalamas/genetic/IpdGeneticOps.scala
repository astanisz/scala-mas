package pl.edu.agh.scalamas.genetic

import pl.edu.agh.ipd.evolution.{MutationFromPaper, Reproduction}
import pl.edu.agh.ipd.game.RepeatedGame
import pl.edu.agh.ipd.model.{Defection, StrategyImpl, Player}
import pl.edu.agh.ipd.utils.{TemporatyConstants, ProbabilityUtils}
import pl.edu.agh.scalamas.app.AgentRuntimeComponent
import pl.edu.agh.scalamas.random.RandomGeneratorComponent

import scala.collection.mutable.ListBuffer

/**
 * Created by Anita on 2015-12-31.
 */

trait IteratedPrisonersDilema extends GeneticProblem {
  this: AgentRuntimeComponent with RandomGeneratorComponent =>

  type Genetic = IpdGeneticOps

  def genetic = new IpdGeneticOps

  class IpdGeneticOps extends GeneticOps[IpdGeneticOps] {

    type Feature = Double
    type Solution = Player
    type Evaluation = Double

    def config = agentRuntime.config.getConfig("genetic.rastrigin")

    val problemSize = config.getInt("problemSize")
    val mutationChance = config.getDouble("mutationChance")
    val mutationRate = config.getDouble("mutationRate")

    def generate = {
      val strategy = new StrategyImpl
      val state = new Defection

      state.setNextIfCooperation(state)
      state.setNextIfDefection(state)
      strategy.addState(state)

      val p: Player = new Player
      p.setStrategy(strategy)
      p
    }


    override def fight(solution1: Solution, solution2: Solution) = {
      val repeated: RepeatedGame = new RepeatedGame
      repeated.play(solution1, solution2)
      List(solution1, solution2)
    }

    def evaluate(solution: Solution) = {
      solution.getPayOff
    }

    // TODO take problemSize into account
    val minimal = 10000.0

    val ordering = Ordering[Double].reverse

    def transform(solution: Solution) = {
      val mutation: MutationFromPaper = new MutationFromPaper
      val mutationProbability: Double = 0.001
      if (ProbabilityUtils.simulateProbability(mutationProbability))
        mutation.mutate(solution)
      else solution
    }

    override def transform(solutions: List[Solution]) =
      reproduction(solutions)

    def transform(solution1: Solution, solution2: Solution) =
      (solution1, solution2)

    // mutateSolutions(recombineSolutions(solution1, solution2))

    def reproduction(solutions: List[Solution]): List[Solution] = {
      val reprod: Reproduction = new Reproduction
      return reprod.reproduct(solutions)
    }


    //    def mutateSolutions(s: (Solution, Solution)) = s match {
    //      case (solution1, solution2) => (mutate(solution1), mutate(solution2))
    //    }


  }

}