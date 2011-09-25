
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

import org.squarewordsolver.{CharToRemove, PuzzleArea, RemoveAdvice}

/**
 * @author abcdef
 * Date: 9/10/11
 * Time: 11:00 AM
 */

/**
 * Given next situation on puzzle field:
 * AB....AB
 * .      .
 * .      .
 * AB....ABCD
 *
 * then ABCD can be replaced with CD: AB cannot be there because A and B should be in upper right and lower left corners
 */
class CollisionByQuadrateFriendlyPairs(
                                        private val puzzleArea: PuzzleArea,
                                        private val friendlyPairs: Set[((Char, Char), (Int, Int), (Int, Int))]
                                        ) extends RemoveAdvice {

  override def getAdvice = {
    val uniqueFriendlyPairs = friendlyPairs.foldLeft(Set[(Char, Char)]())((aggregation, currFriendlyPair) => {
      val currPairOfChars = currFriendlyPair._1
      if (aggregation contains currPairOfChars.swap)
        aggregation
      else
        aggregation + currPairOfChars
    })

    val allCoordinates = for {pairOfChars <- uniqueFriendlyPairs
                              withSwapped = Set(pairOfChars, pairOfChars.swap)
                              allCoordinatesForThisPair = friendlyPairs.filter(withSwapped contains _._1)
                                .foldLeft(Set[(Int, Int)]())((aggregation, currFriendlyPair) =>
                                aggregation + currFriendlyPair._2 + currFriendlyPair._3)}
    yield (pairOfChars, allCoordinatesForThisPair)

    val interestingCoordinates = allCoordinates.filter(_._2.size > 2)

    val correctQuadrateCandidates = for {
      currPair <- interestingCoordinates
      pairOfChars = currPair._1
      setOfCoordinates = currPair._2
      basicsCoordinate <- setOfCoordinates
      first = setOfCoordinates.filter(el => el._1 == basicsCoordinate._1 && el._2 != basicsCoordinate._2)
      second = setOfCoordinates.filter(el => el._2 == basicsCoordinate._2 && el._1 != basicsCoordinate._1)
      if (first.size == 1 && second.size == 1)
    }
    yield (pairOfChars, Set(basicsCoordinate, first.head, second.head))

    //println(correctQuadrateCandidates)

    val charsToRemove = for {
      correctCandidate <- correctQuadrateCandidates
      currSetOfPairs = {
        Set(correctCandidate._1._1, correctCandidate._1._2)
      }

      toList = correctCandidate._2.toList
      fourthCornerCoords = (toList(2)._1, toList(1)._2)
      fourthCornerCell = {
        println(puzzleArea.at(2,6).possibleVals & currSetOfPairs)
        puzzleArea.at(fourthCornerCoords._1, fourthCornerCoords._2)
      }
      if (fourthCornerCell.isNotSet)
      charsToRemove = currSetOfPairs & fourthCornerCell.possibleVals
      if (!charsToRemove.isEmpty)
    }

    yield CharToRemove(charsToRemove.head, fourthCornerCoords)

    println(charsToRemove)

    charsToRemove.toList
  }
}