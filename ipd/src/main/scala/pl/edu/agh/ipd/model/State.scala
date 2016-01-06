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
import pl.edu.agh.ipd.model.State.Color.Color

object State {

  object Color extends Enumeration {
    type Color = Value
    val RED, BLUE = Value
  }

}

abstract class State {
  private var nextIfCooperation: State = null
  private var nextIfDefection: State = null

  def getAction: Action

  def getCopy: State

  def getColor: Color

  def relinkTransition(fromState: State, toState: State) {
    if (getNextIfCooperation eq fromState) {
      setNextIfCooperation(toState)
    }
    if (getNextIfDefection eq fromState) {
      setNextIfDefection(toState)
    }
  }

  def getNextIfCooperation: State = {
    return nextIfCooperation
  }

  def setNextIfCooperation(nextIfCooperation: State) {
    this.nextIfCooperation = nextIfCooperation
  }

  def getNextIfDefection: State = {
    return nextIfDefection
  }

  def setNextIfDefection(nextIfDefection: State) {
    this.nextIfDefection = nextIfDefection
  }
}