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

package pl.edu.agh.scalamas.emas

import pl.edu.agh.scalamas.genetic.GeneticOps
import pl.edu.agh.scalamas.mas.LogicTypes
import pl.edu.agh.scalamas.mas.LogicTypes.{Coordinates, Population, Behaviour}

/**
 * Holder for the types of agents and behaviours used in EMAS.
 */
object EmasTypes {

  case class Agent[G <: GeneticOps[G]](val solution: G#Solution, val fitness: G#Evaluation, var energy: Int, var position:Coordinates, var evolutionState:Int) extends LogicTypes.Agent

  case class Death(capacity: Int) extends Behaviour

  case class Fight(capacity: Int) extends Behaviour

  case class Reproduction(capacity: Int) extends Behaviour

  case class Mutation(capacity: Int) extends Behaviour

  def checked[G <: GeneticOps[G]](pop: Population) = pop.collect { case a: EmasTypes.Agent[G] => a}
}