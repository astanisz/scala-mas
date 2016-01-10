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
package pl.edu.agh.scalamas.emas.fight

import pl.edu.agh.scalamas.app.AgentRuntimeComponent
import pl.edu.agh.scalamas.emas.EmasTypes.Agent
import pl.edu.agh.scalamas.genetic.GeneticProblem

import scala.math._

trait IpdFightStrategy extends FightStrategy {
  this: AgentRuntimeComponent with GeneticProblem =>

  def fightStrategy = IpdFightStrategy

  object IpdFightStrategy extends Fight {
    val fightTransfer = agentRuntime.config.getInt("emas.fightTransfer")

    def apply(agents: List[Agent[Genetic]]) = agents match {
      case List(a) => List(a)
      case List(a, b) =>

        val newSolutions = genetic.fight(a.solution, b.solution)
        val newA: Genetic#Solution=newSolutions._1
        val newB: Genetic#Solution=newSolutions._2
//        val AtoBTransfer =
//          if (genetic.ordering.lt(a.fitness, b.fitness))
//            min(fightTransfer, a.energy)
//          else
//            -min(fightTransfer, b.energy)
//        List(a.copy(energy = a.energy - AtoBTransfer), b.copy(energy = b.energy + AtoBTransfer))

        List(Agent[Genetic](newA, genetic.evaluate(newA), 0),Agent[Genetic](newB, genetic.evaluate(newB), 0))
    }


  }

}