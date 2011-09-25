
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

package org.squarewordsolver.impl

import collection.mutable.ListBuffer
import org.squarewordsolver.{CharToRemove, PuzzleArea, RemoveAdvice}

/**
 * @author MrKeyholder
 * Date: 7/24/11
 * Time: 4:28 PM
 */

class CollisionByDiagonalFriendlyPairs(
                                        private val puzzleArea: PuzzleArea,
                                        private val friendlyPairs: Set[((Char, Char), (Int, Int), (Int, Int))]
                                        ) extends RemoveAdvice {

  private val dimension = puzzleArea.dimension

  override def getAdvice = {

    var found = ListBuffer[CharToRemove]()
    def updateFound(i: Int, j: Int, c: Char) {
      found += CharToRemove(c, (i, j))
    }

    def updateTriangleLike(firstChar: Char, secondChar: Char, i1: Int, j1: Int, i2: Int, j2: Int) {
      // main diagonal
      val magicSet = Set(firstChar, secondChar)

      def updateRemovableValuesFromSet(set: Set[Char], i: Int, j: Int) {
        set.foreach {
          case charToRemove => updateFound(i, j, charToRemove)
        }
      }

      updateRemovableValuesFromSet(puzzleArea.at(i1, j1).possibleVals -- magicSet, i1, j1)

      updateRemovableValuesFromSet(puzzleArea.at(i2, j2).possibleVals -- magicSet, i2, j2)

      // triangle searches
      // if
      // AB  *  *
      //  *  *  *
      // ABG * AB
      // and rows with AB are on main diagonal
      // then A and B can be removed from cell ABG because A & B must be
      // already in one of the two AB cells
      updateRemovableValuesFromSet(puzzleArea.at(i1, j2).possibleVals & magicSet, i1, j2)

      updateRemovableValuesFromSet(puzzleArea.at(i2, j1).possibleVals & magicSet, i2, j1)
    }

    friendlyPairs.foreach {
      // TODO this code does not work correctly if dimension is even
      // TODO fix
      case ((firstChar, secondChar), (i1, j1), (i2, j2)) if i1 == j1 && i2 == j2 => {
        updateTriangleLike(firstChar, secondChar, i1, j1, i2, j2)
      }

      case ((firstChar, secondChar), (i1, j1), (i2, j2)) if i1 + j1 + 1 == dimension && i2 + j2 + 1 == dimension => {
        updateTriangleLike(firstChar, secondChar, i1, j1, i2, j2)
      }

      case _ =>
    }

    found.toList
  }
}