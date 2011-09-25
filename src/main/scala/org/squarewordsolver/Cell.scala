
/*
 * Copyright (C) 2011 MrKeyholder https://github.com/MrKeyholder
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

package org.squarewordsolver

/**
 * single item in Squareword field
 *
 * @author MrKeyholder
 */

class Cell(
            var item: Char,
            val coordX: Int,
            val coordY: Int,
            var possibleVals: Set[Char] = Set[Char]()
            ) {

  /**
   * set the value explicitly based on some complicated algorithm
   */
  def setFound(c: Char) {
    assume(c != ' ', "invalid char '%s' to set explicitly".format(c))
    item = c
    possibleVals = Set[Char]()
  }


  def isSet = (' ' != item)

  def is(i: Char) = (i == item)

  def isNotSet = !isSet

  def possibleNotInitialized = (is(' ') && possibleVals.isEmpty)

  def isPossibleChar(char: Char) = possibleVals.contains(char)

  def tryToSetFound() = {
    if (possibleVals.size == 1) {
      item = possibleVals.head
      possibleVals = Set[Char]()
      true
    } else false
  }

  /**
   * update possible values based on some complicated algorithm
   */
  def removePossibleVal(toRemove: Char) {
    assume(possibleVals.contains(toRemove), "%s does not contain char %s".format(this, toRemove))
    possibleVals = possibleVals - toRemove
  }

  override def toString = {
    "{%s}<%s, %s>".format(item.toUpper, coordX, coordY)
  }

}

object Cell {
  def empty(coordX:Int, coordY:Int) = new Cell(' ', coordX, coordY)
  val EMPTY = new Cell(' ', -1, -1)
}