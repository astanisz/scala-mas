package pl.edu.agh.ipd.evolution

import pl.edu.agh.ipd.model.{Player, State, Strategy, StrategyImpl}
import pl.edu.agh.ipd.utils.ProbabilityUtils

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by Anita on 2016-05-04.
  */
class Crossover (ALPHA: Double) {
    def cross(solutions: List[Player]): List[Player] = {
        val newPopulation: ListBuffer[Player] = ListBuffer[Player]()

        assert(solutions.size == 2, "Crossover can be performed only between two players, was " + solutions.size)
        if (ProbabilityUtils.simulateProbability(0.5)) {
            val first = solutions(0)
            val second = solutions(1)

            if (first.getPayOff > second.getPayOff)
                newPopulation.append(getCopyOf(first))
            else
                newPopulation.append(getCopyOf(second))

            val crossoverResult = cross(first, second)

            newPopulation.append(crossoverResult)
            newPopulation.toList
        } else {
            return new Reproduction(ALPHA).reproduct(solutions)
        }

    }

    private def cross(first: Player, second: Player): Player = {
        val firstPlayerStates = first.getStrategyCopy.getStates
        val secondPlayerStates = second.getStrategyCopy.getStates
        val firstPart: ListBuffer[State] = getHalfOfStrategy(firstPlayerStates)
        val secondPart: ListBuffer[State] = getHalfOfStrategy(secondPlayerStates)

        val newStrategy: ListBuffer[State] = new ListBuffer[State]()
        newStrategy.appendAll(firstPart)
        newStrategy.appendAll(secondPart)

        for (state <- newStrategy) {
            if (!newStrategy.contains(state.getNextIfCooperation)) {
                state.setNextIfCooperation(pickStateAtRandom(newStrategy))
            }
            if (!newStrategy.contains(state.getNextIfDefection)) {
                state.setNextIfDefection(pickStateAtRandom(newStrategy))
            }
        }
        val newPlayer: Player = new Player
        val strategy: Strategy = new StrategyImpl(newStrategy)
        newPlayer.setStrategy(strategy)

        newPlayer
    }

    private val rand = new Random

    private def pickStateAtRandom(states: ListBuffer[State]): State = {
        states(rand.nextInt(states.size))
    }

    def getHalfOfStrategy(states: ListBuffer[State]): ListBuffer[State] = {
        if (states.size > 1) {
            return states.splitAt(states.size / 2)._1
        } else {
            return states;
        }
    }

    private def getCopyOf(player: Player): Player = {
        val copy: Player = new Player
        copy.setStrategy(player.getStrategyCopy)
        copy.setPayoff(player.getPayOff)
        return copy
    }


}
