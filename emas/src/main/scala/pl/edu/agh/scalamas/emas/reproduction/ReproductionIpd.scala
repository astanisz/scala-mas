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

import pl.edu.agh.ipd.utils.{TemporatyConstants, ProbabilityUtils}
import pl.edu.agh.scalamas.app.AgentRuntimeComponent
import pl.edu.agh.scalamas.emas.EmasTypes.Agent
import pl.edu.agh.scalamas.genetic.GeneticProblem

import scala.collection.mutable.ListBuffer
import scala.math._


trait IpdReproductionStrategy extends ReproductionStrategy {
  this: AgentRuntimeComponent with GeneticProblem =>

  def reproductionStrategy = IpdReproductionImpl

  object IpdReproductionImpl extends Reproduction {
    val reproductionTransfer = agentRuntime.config.getInt("emas.reproductionTransfer")

    def apply(agents: List[Agent[Genetic]]) = agents match {
      case List(a) =>
        val s = genetic.transform(a.solution)
        val f = genetic.evaluate(s)
       // val e = min(reproductionTransfer, a.energy)
        //List(a.copy(energy = a.energy - e), Agent[Genetic](s, f, e))
//       List( Agent[Genetic](s, f, 0))
        val e = min(reproductionTransfer, a.energy)
        List(a.copy(energy = a.energy - e), Agent[Genetic](s, f, e))
      case _ =>
        var solutions: List[Genetic#Solution] = agents.map(x => x.solution)
        solutions = genetic.transform(solutions)
        val newPopulation: ListBuffer[Agent[Genetic]] = ListBuffer[Agent[Genetic]]()
        for (s <- solutions) {
          val f1 = genetic.evaluate(s)
          val agent = Agent[Genetic](s, f1, 0)
          newPopulation.+=(agent)
        }
        newPopulation.toList
    }

  }

}