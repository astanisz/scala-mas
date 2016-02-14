package pl.edu.agh.ipd.game

import pl.edu.agh.ipd.model.Player
import pl.edu.agh.ipd.model.Action.Action
import pl.edu.agh.ipd.utils.{ProbabilityUtils}


/**
 * Created by Anita on 2016-01-01.
 */
class RepeatedGame(val DELTA: Double) {

  def play(players: Tuple2[Player, Player]): (Player, Player) = {
    val player1 = players._1
    val player2 = players._2
    player1.setPayoff(0)
    player2.setPayoff(0)
    var newPlayers: (Player, Player) = (player1, player2)
    do {
      newPlayers = playRound(newPlayers._1, newPlayers._2)
    } while (ProbabilityUtils.simulateProbability(DELTA))

    newPlayers
  }

  /**
   * For repeated game payoffs are normalized.
   */
  private def normalizePayoff(player: Player) {
    player.setPayoff(player.getPayOff * (1 - DELTA))
  }

  private def playRound(player1: Player, player2: Player): (Player, Player) = {
    val action1: Action = player1.getStrategy.getCurrentState.getAction
    val action2: Action = player2.getStrategy.getCurrentState.getAction
    val game: GameImpl = new GameImpl()
    val newPlayers=game.play((player1, player2), (action1, action2))
    val newPlayer1=newPlayers._1
    val newPlayer2=newPlayers._2
    newPlayer1.getStrategy.goToNextState(action2)
    newPlayer2.getStrategy.goToNextState(action1)
    (newPlayer1, newPlayer2)
  }
}