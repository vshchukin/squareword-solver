
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
import org.squarewordsolver.{LinesCache, PuzzleArea, Cell}

/**
 * find the pairs of elements
 * that are coordinates of two cells
 * where the only possible position for
 * at least two of characters
 * for example: AB ....... AB,
 * and neither A nor B can exist in this
 * row (column, diagonal)
 *
 * @author MrKeyholder
 * Date: 7/23/11
 * Time: 7:55 PM
 */

class FriendlyPairsFinder(
                           private val puzzleArea: PuzzleArea
                           ) {
  private val dimension = puzzleArea.dimension
  private val linesCache = new LinesCache(dimension, (i, j) => puzzleArea.at(i, j))

  /**
   * return set of such pairs:
   * each element of set is a pair of three pairs:
   * 1st - pair of chars that creates "friendly pair"
   * 2nd - coordinate of first char
   * 3rd - coordinate of second char
   */
  def get: Set[((Char, Char), (Int, Int), (Int, Int))] = {

    var onlies = new ListBuffer[((Char, Char), (Int, Int), (Int, Int))]()
    linesCache.getEverythingConstrained.view.zipWithIndex.foreach {
      case (value, index) => {
        val possiblesSet = value.foldLeft(Set[Char]())(_ ++ _.possibleVals)
        onlies ++= findFriendlyPairs(value, index, possiblesSet)
      }
    }

    onlies.toSet
  }

  def findFriendlyPairs(constrainedSeq: List[Cell], index: Int, possiblesSet: Set[Char])
  : Set[((Char, Char), (Int, Int), (Int, Int))] = {

    def getIfOnlyTwo(c: Char): Set[Char] = {
      constrainedSeq.filter(_.possibleVals.contains(c)).size match {
        case 2 => Set(c)
        case _ => Set()
      }
    }

    val onlyTwoChars = possiblesSet.foldLeft(Set[Char]())(_ ++ getIfOnlyTwo(_))


    val distinctPairsOfOnlyTwoChars = onlyTwoChars.foldLeft(Set[(Char, Char)]())((sum, next) =>
      sum ++ onlyTwoChars.filter(_ != next).map(el => if (!sum.contains((next, el))) (el, next) else (next, el)))
    //printf("Index: %s, onlyTwoChars: %s, distinctPairsOfOnlyTwoChars: %s\n", index, onlyTwoChars.toString(), distinctPairsOfOnlyTwoChars)

    var onlies = new ListBuffer[((Char, Char), (Int, Int), (Int, Int))]()
    distinctPairsOfOnlyTwoChars.foreach {
      case (first, second) => {
        def theFilter(c: Cell): Boolean = Set(first, second).forall(c.possibleVals.contains)
        val firstIdx = constrainedSeq.indexWhere(theFilter(_))
        val secondIdx = constrainedSeq.indexWhere(theFilter(_), firstIdx + 1)
        if (-1 != firstIdx && -1 != secondIdx) {
          //printf("!!!found THE pair: (%s, %s), [%s, %s]\n", LinesCache.normalCoordinates(index, firstIdx, dimension), LinesCache.normalCoordinates(index, secondIdx, dimension), first, second)
          onlies += (((first, second), LinesCache.normalCoordinates(index, firstIdx, dimension), LinesCache.normalCoordinates(index, secondIdx, dimension)))
        }
      }
    }
    onlies.toSet
  }
}