package pl.edu.agh.scalamas.genetic

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import pl.edu.agh.ipd.evolution.{Crossover, MutationFromPaper, Reproduction}
import pl.edu.agh.ipd.game.RepeatedGame
import pl.edu.agh.ipd.model._
import pl.edu.agh.ipd.utils.ProbabilityUtils
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

        def config = agentRuntime.config.getConfig("genetic.ipd")

        val mutationProbability = config.getDouble("mutationProbability")

        // This obscure code gets environment variable DELTA if defined. In other case delta is read from config
        val delta = scala.util.Properties.envOrElse("DELTA", config.getString("delta")).toDouble

        // Similar hack for alpha
        val alpha = scala.util.Properties.envOrElse("ALPHA", config.getString("alpha")).toDouble

        def generate = {
            val strategy = new StrategyImpl(new ListBuffer[State])
            val state = new Defection

            state.setNextIfCooperation(state)
            state.setNextIfDefection(state)
            strategy.addState(state)

            val p: Player = new Player
            p.setStrategy(strategy)
            p
        }


        override def fight(solution1: Solution, solution2: Solution) = {
            val repeated: RepeatedGame = new RepeatedGame(delta)
            val newSolutions = repeated.play(solution1, solution2)
            newSolutions
        }

        def evaluate(solution: Solution) = {
            solution.getPayOff
        }

        // TODO take problemSize into account
        val minimal = 0.0

        val ordering = Ordering[Double].reverse

        def transform(solution: Solution) = {
            val mutation: MutationFromPaper = new MutationFromPaper
            if (ProbabilityUtils.simulateProbability(mutationProbability))
                mutation.mutate(solution)
            else solution
        }

        override def transform(solutions: List[Solution]) =
            reproduction(solutions)

        def transform(solution1: Solution, solution2: Solution) =
            (solution1, solution2)


        def reproduction(solutions: List[Solution]): List[Solution] = {
            val reprod: Crossover = new Crossover(alpha)
            val sum = solutions.map(p => p.getPayOff).sum;
            val average_payoff: Double = sum / solutions.size;
            //            saveAveragePayoff(average_payoff.toString + "\n")
            reprod.cross(solutions)
        }

        override def evaluateFinalResult(solutions: List[Solution]): Double = {
            var cooperationSum = 0
            var statesNumber = 0
            for (s <- solutions) {
                val states: ListBuffer[State] = s.getStrategy.getStates
                statesNumber += states.size
                cooperationSum += states.toStream.count(state => state.getAction.eq(Action.COOPERATION))
            }
            cooperationSum * 100.0 / statesNumber
        }

        def saveAveragePayoff(lines: String) {
            val writer = new FileWriter(new File("fitness" + delta + "_" + alpha + ".txt"), true)

            writer.write(lines)
            writer.close()
        }
    }

}
