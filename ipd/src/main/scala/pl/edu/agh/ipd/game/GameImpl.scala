/*
 * Copyright 2013 - 2015, Daniel Krzywicki <daniel.krzywicki@agh.edu.pl>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package pl.edu.agh.ipd.game

import pl.edu.agh.ipd.model.Action.Action
import pl.edu.agh.ipd.model.Player
import pl.edu.agh.ipd.model.Action

class GameImpl {
  // Values from article
  object Payoff {
    val R = 2
    val S = 0
    val T = 3
    val P = 1
  }

  def play(players: Tuple2[Player,Player], actions: Tuple2[Action,Action]) : (Player, Player) ={
    val player1: Player = players._1
    val player2: Player = players._2
    val action1: Action = actions._1
    val action2: Action = actions._2
    setPayoffs(player1, player2, action1, action2)
    (player1,player2)
  }

  private def setPayoffs(player1: Player, player2: Player, action1: Action, action2: Action): (Player, Player) = {
    if (action1 == Action.COOPERATION) {
      if (action2 == Action.COOPERATION) {
        player1.addPayOff(Payoff.R)
        player2.addPayOff(Payoff.R)
      }
      else if (action2 == Action.DEFECTION) {
        player1.addPayOff(Payoff.S)
        player2.addPayOff(Payoff.T)
      }
    }
    else {
      if (action2 == Action.COOPERATION) {
        player1.addPayOff(Payoff.T)
        player2.addPayOff(Payoff.S)
      }
      else if (action2 == Action.DEFECTION) {
        player1.addPayOff(Payoff.P)
        player2.addPayOff(Payoff.P)
      }
    }
    (player1,player2)
  }
}