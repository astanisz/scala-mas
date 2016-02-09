package pl.edu.agh.ipd.evolution

import pl.edu.agh.ipd.model.Action.Action
import pl.edu.agh.ipd.model.{Action, Cooperation, Defection, Player, State}
import pl.edu.agh.ipd.utils.ProbabilityUtils

import scala.collection.mutable.ListBuffer
import scala.util.Random

object MutationFromPaper {
  private val halfChance: Double = 0.5
}

class MutationFromPaper {
  private val rand = new Random


  def mutate(player: Player):Player= {
    mutatePlayer(player)
  }

  private def mutatePlayer(player: Player):Player= {
    if (player.getStrategy.getStates.size == 1) addState(player)
    else {
      if (ProbabilityUtils.simulateProbability(MutationFromPaper.halfChance)) addState(player)
      else deleteState(player)
    }
    player
  }

  private def addState(player: Player) {
    val mutatedState: State = pickStateAtRandom(player)
    val newState: State = createStateAtRandom
    player.getStrategy.addState(newState)
    makeTransition(pickTransitionAtRandom, mutatedState, newState)
    createTransitions(player, newState)
  }

  private def deleteState(player: Player) {
    if (player.getStrategy.getStates.size <= 1) return
    val deletedState: State = pickStateAtRandom(player)

    for (state <- player.getStrategy.getStates) {
      var destinationState: State = null
      do {
        destinationState = pickStateAtRandom(player)
      } while (destinationState eq deletedState)
      state.relinkTransition(deletedState, destinationState)
    }
    player.getStrategy.removeState(deletedState)
  }

  private def makeTransition(transition: Action, fromState: State, toState: State) {
    if (transition eq Action.COOPERATION) {
      fromState.setNextIfCooperation(toState)
    }
    else {
      fromState.setNextIfDefection(toState)
    }
  }

  private def createTransitions(player: Player, state: State) {
    val goToOnCooperation: State = pickStateAtRandom(player)
    val goToOnDefection: State = pickStateAtRandom(player)
    state.setNextIfCooperation(goToOnCooperation)
    state.setNextIfDefection(goToOnDefection)
  }

  private def pickTransitionAtRandom: Action = {
    if (ProbabilityUtils.simulateProbability(MutationFromPaper.halfChance)) {
       Action.COOPERATION
    }
    else {
       Action.DEFECTION
    }
  }

  private def pickStateAtRandom(player: Player): State = {
    val states: ListBuffer[State] = player.getStrategy.getStates
     states(rand.nextInt(states.size))
  }

  private def createStateAtRandom: State = {
    if (ProbabilityUtils.simulateProbability(MutationFromPaper.halfChance)) {
      new Cooperation
    }
    else {
      new Defection
    }
  }
}