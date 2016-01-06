package pl.edu.agh.ipd.evolution

import pl.edu.agh.ipd.model.Player
import pl.edu.agh.ipd.utils.{TemporatyConstants, ProbabilityUtils}

import scala.collection.mutable.ListBuffer

/**
 * Created by Anita on 2016-01-03.
 */
class Reproduction {

  def reproduct(solutions: List[Player] ) : List[Player] =
  {
    //copy players to a parent population
    val newPopulation: ListBuffer[Player] = ListBuffer[Player]()
    var payoffsSum: Double = 0.0

    for (player <- solutions) {
      payoffsSum += player.getPayOff
    }

    for (player <- solutions) {
      player.setProbability(player.getPayOff / payoffsSum)
    }

    var i: Int = 1
    while (i < solutions.size) {

      val parent: Player = pickParentProportionalToPayoff(solutions)
      newPopulation.insert(i - 1, getCopyOf(parent))
      if (ProbabilityUtils.simulateProbability(TemporatyConstants.ALPHA)) {
        newPopulation.insert(i, getCopyOf(parent))
      }
      else {
        val alternativeParent: Player = pickParentProportionalToPayoff(solutions)
        newPopulation.insert(i, getCopyOf(alternativeParent))
      }
      i += 2
    }
    newPopulation.toList
  }

  private def getCopyOf(player: Player): Player = {
    val copy: Player = new Player
    copy.setStrategy(player.getStrategyCopy)
    copy.setPayoff(player.getPayOff)
    return copy
  }

  private def pickParentProportionalToPayoff(players: List[Player]): Player = {
    val p: Double = Math.random
    var cumulativeProbability: Double = 0.0

    for (player <- players) {
      cumulativeProbability += player.getProbability
      if (p <= cumulativeProbability) {
        return player
      }
    }
    //FIXME: null?
    return players.head
  }

}