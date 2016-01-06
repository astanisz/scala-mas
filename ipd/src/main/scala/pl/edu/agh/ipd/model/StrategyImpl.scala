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
package pl.edu.agh.ipd.model

import pl.edu.agh.ipd.model.Action.Action
import pl.edu.agh.ipd.utils.ProbabilityUtils

import scala.collection.mutable.ListBuffer

class StrategyImpl extends Strategy {
  private var states: ListBuffer[State] = ListBuffer [State]()
  private var current: State = null

  def getCurrentState: State = {
    return current
  }

  def goToNextState(action: Action) {
    if (action == Action.COOPERATION) current = current.getNextIfCooperation
    else current = current.getNextIfDefection
  }

  def getStates: ListBuffer[State] = {
    return states
  }

  def addState(state: State) {
    if (states.isEmpty) current = state
    states.+=(state)
  }

  /**
   * Removes state from automata. If removed state is current state, moves current to next-if-cooperation or to
   * next-if-defection with equal probability. If next-ifs don't exists (it is the last state in a strategy) - goes to
   * the next (or previous) state on the states list.
   */
  def removeState(state: State) {
    if (state == current) {
      moveCurrent(state)
    }
    states-=state
  }

  def resetHistory {
    if (states.isEmpty) throw new IllegalStateException("Cannot reset strategy history if list of states is empty")
    current = states.head
  }

  def getCopy: Strategy = {
    val copy: Strategy = new StrategyImpl
    states.foreach{(s: State)=> copy.addState(s.getCopy)}

    var i: Int = 0
    while (i < states.size) {
        copyLinks(copy.getStates(i), states(i), copy.getStates)
        i=i+1
    }

    return copy
  }

  private def copyLinks(stateFromCopy: State, originalState: State, copyStates: ListBuffer[State]) {
    stateFromCopy.setNextIfCooperation(copyStates(states.indexOf(originalState.getNextIfCooperation)))
    stateFromCopy.setNextIfDefection(copyStates(states.indexOf(originalState.getNextIfDefection)))
  }

  private def moveCurrent(state: State) {
    if (ProbabilityUtils.simulateProbability(0.5)) current = state.getNextIfCooperation
    else current = state.getNextIfDefection
    if (current == null) {
      if (states.size > 1) {
        val index: Int = states.indexOf(state)
        if (index + 1 <= states.size - 1) current = states(index + 1)
        else current = states(index - 1)
      }
      else current = null
    }
  }
}