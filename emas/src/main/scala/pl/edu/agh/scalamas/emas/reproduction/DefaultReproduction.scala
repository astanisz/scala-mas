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
package pl.edu.agh.scalamas.emas.reproduction

import pl.edu.agh.scalamas.app.AgentRuntimeComponent
import pl.edu.agh.scalamas.emas.EmasTypes.Agent
import pl.edu.agh.scalamas.genetic.GeneticProblem

import scala.math._

/**
 * Default EMAS reproduction strategy. Two children agents are created out of two parent ones. A binary genetic operator is used
 * to create the children solutions. The parents also give some of their energy to the children.
 *
 * Parameters:
 *  - emas.reproductionTransfer - the amount of energy to transfer from parent to child.
 */
trait DefaultReproduction extends ReproductionStrategy {
  this: AgentRuntimeComponent with GeneticProblem =>

  def reproductionStrategy = DefaultReproductionImpl

  object DefaultReproductionImpl extends Reproduction {
    val reproductionTransfer = agentRuntime.config.getInt("emas.reproductionTransfer")

    def apply(agents: List[Agent[Genetic]]) = agents match {
      case List(a) =>
        val s = genetic.transform(a.solution)
        val f = genetic.evaluate(s)
        val e = min(reproductionTransfer, a.energy)
        List(a.copy(energy = a.energy - e), Agent[Genetic](s, f, e, a.position,a.evolutionState))
      case List(a1, a2) =>
        val (s1, s2) = genetic.transform(a1.solution, a2.solution)
        val (f1, f2) = (genetic.evaluate(s1), genetic.evaluate(s2))
        val (e1, e2) = (min(reproductionTransfer, a1.energy), min(reproductionTransfer, a2.energy))
        List(a1.copy(energy = a1.energy - e1), a2.copy(energy = a2.energy - e2), Agent[Genetic](s1, f1, e1, a1.position,a1.evolutionState), Agent[Genetic](s2, f2, e2, a2.position,a2.evolutionState))
    }
  }

}