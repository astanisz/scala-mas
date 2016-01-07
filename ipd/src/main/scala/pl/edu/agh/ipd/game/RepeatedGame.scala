package pl.edu.agh.ipd.game

import pl.edu.agh.ipd.model.Player
import pl.edu.agh.ipd.model.Action.Action
import pl.edu.agh.ipd.utils.{ProbabilityUtils}


/**
 * Created by Anita on 2016-01-01.
 */
class RepeatedGame(val DELTA: Double) {

  def play(players: Tuple2[Player, Player]): List[Player] = {
    val player1 = players._1
    val player2 = players._2
    player1.setPayoff(0)
    player2.setPayoff(0)

    do {
      playRound(player1, player2)
    } while (ProbabilityUtils.simulateProbability(DELTA))

    List(player1, player2)
  }

  /**
   * For repeated game payoffs are normalized.
   */
  private def normalizePayoff(player: Player) {
    player.setPayoff(player.getPayOff * (1 - DELTA))
  }

  private def playRound(player1: Player, player2: Player) {
    val action1: Action = player1.getStrategy.getCurrentState.getAction
    val action2: Action = player2.getStrategy.getCurrentState.getAction
    val game: GameImpl = new GameImpl()
    game.play((player1, player2), (action1, action2))
    player1.getStrategy.goToNextState(action2)
    player2.getStrategy.goToNextState(action1)
  }
}


