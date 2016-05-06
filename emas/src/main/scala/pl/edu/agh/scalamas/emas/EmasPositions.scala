package pl.edu.agh.scalamas.emas

import pl.edu.agh.scalamas.mas.logic.PositionStrategy

/**
  * Created by Anita on 2016-02-21.
  */
trait EmasPositions extends PositionStrategy {
  def positionStrategy = DefaultEmasPosition

  object DefaultEmasPosition extends PositionProvider {

    def positionFunction = {
      case (agent, arenas, behaviour) => {
        var position = agent.position.y % arenas
//        if (Math.random < 0.5)
//          position = agent.position.x % arenas
        position
        }
      }

  }

}
