package pl.edu.agh.scalamas.mas.logic

import pl.edu.agh.scalamas.mas.LogicTypes.PositionFunction


/**
* Created by Anita on 2016-02-21.
*/
trait PositionStrategy {

  def positionStrategy: PositionProvider

  trait PositionProvider {

    def positionFunction: PositionFunction

  }

}
