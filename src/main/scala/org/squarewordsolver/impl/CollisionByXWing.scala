
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

import org.squarewordsolver._

/**
 * @author MrKeyholder
 * Date: 9/11/11
 * Time: 11:32 AM
 */

/**
 * Given unset symbols
 * 1        A
 * 2    A   A
 * 3    A
 * 4    A   A
 *
 * in rows 2 and 4 there are only two possible A and they are in same columns
 * then all other A can be removed from these two columns. In current example
 * possible values to be removed are A in row 1 and row 3
 *
 */
class CollisionByXWing(
                        private val puzzleArea: PuzzleArea
                        ) extends RemoveAdvice {
  private val internalLinesCache = new LinesCache(puzzleArea)

  def getAdvice = {

    puzzleArea.getUnfinishedChars.foldLeft(List[CharToRemove]())((aggr, curr) => xWingCollisionPerChar(curr) ::: aggr)
  }

  private def xWingCollisionPerChar(char: Char) = {
    val cacheOnlyWithChar = new LinesCache(puzzleArea).onlyWithChar(char)

    def getOnlyWithTwoChars2(theList: List[List[Cell]]) = theList.foldLeft(List[List[Cell]]())((summ, curr) => {
      val twos = curr filter (_.isSet)
      if (twos.size == 2)
        twos :: summ
      else
        summ
    })
    def toPairOfCells(listOfTwos: List[Cell]) = (listOfTwos.head, listOfTwos.tail.head)

    def rowsFilteringCondition(firstPair: (Cell, Cell), secondPair: (Cell, Cell)) = firstPair._1.coordY != secondPair._1.coordY &&
      ((firstPair._1.coordX, firstPair._2.coordX)) == ((secondPair._1.coordX, secondPair._2.coordX))
    def colsFilteringCondition(firstPair: (Cell, Cell), secondPair: (Cell, Cell)) = firstPair._1.coordX != secondPair._1.coordX &&
      ((firstPair._1.coordY, firstPair._2.coordY)) == ((secondPair._1.coordY, secondPair._2.coordY))

    def rowsInsertingCondition(firstPair: (Cell, Cell), secondPair: (Cell, Cell)) = firstPair._1.coordY > secondPair._1.coordY
    def colsInsertingCondition(firstPair: (Cell, Cell), secondPair: (Cell, Cell)) = firstPair._1.coordX < secondPair._1.coordX

    val rowsOnlyWithTwoChars2 = getOnlyWithTwoChars2(cacheOnlyWithChar.getRows)
    val colsOnlyWithTwoChars2 = getOnlyWithTwoChars2(cacheOnlyWithChar.getColumns)

    def findXWings(
                    collOnlyWithTwoChars: List[List[Cell]],
                    filteringCond: ((Cell, Cell), (Cell, Cell)) => Boolean,
                    insertingCond: ((Cell, Cell), (Cell, Cell)) => Boolean
                    ) = collOnlyWithTwoChars.foldLeft(Set[(Cell, Cell, Cell, Cell)]())((aggregated, current) => {
      val currentCells = toPairOfCells(current)
      val secondPositionsList = collOnlyWithTwoChars.filter(list => filteringCond(toPairOfCells(list), currentCells))
      assume(secondPositionsList.isEmpty || secondPositionsList.size == 1, "Illegal state, not valid squareword")
      if (!secondPositionsList.isEmpty) {
        val theOtherz = toPairOfCells(secondPositionsList.head)
        if (insertingCond(theOtherz, currentCells))
          aggregated + ((currentCells._1, currentCells._2, theOtherz._1, theOtherz._2))
        else
          aggregated + ((theOtherz._1, theOtherz._2, currentCells._1, currentCells._2))
      } else {
        aggregated
      }
    })

    val actualXWingRows = findXWings(
      rowsOnlyWithTwoChars2,
      rowsFilteringCondition,
      rowsInsertingCondition
    )

    val actualXWingCols = findXWings(
      colsOnlyWithTwoChars2,
      colsFilteringCondition,
      colsInsertingCondition
    )

    getCharsToRemoveByActualXWings(
      char,
      actualXWingRows,
      internalLinesCache.getColumn,
      (quad: (Cell, Cell, Cell, Cell)) => (quad._1.coordX, quad._2.coordX),
      (quad: (Cell, Cell, Cell, Cell)) => Set(quad._1.coordY, quad._3.coordY),
      (c: Cell) => c.coordY
    ) ::: getCharsToRemoveByActualXWings(
      char,
      actualXWingCols,
      internalLinesCache.getRow,
      (quad: (Cell, Cell, Cell, Cell)) => (quad._1.coordY, quad._2.coordY),
      (quad: (Cell, Cell, Cell, Cell)) => Set(quad._2.coordX, quad._4.coordX),
      (c: Cell) => c.coordX
    )
  }

  private def getCharsToRemoveByActualXWings(
                                              char: Char,
                                              xWings: Set[(Cell, Cell, Cell, Cell)],
                                              getColOrRowByIndex: Int => List[Cell],
                                              getWhereToDelete: ((Cell, Cell, Cell, Cell)) => (Int, Int),
                                              getWhereToIgnore: ((Cell, Cell, Cell, Cell)) => Set[Int],
                                              theFilter: Cell => Int) = {
    def toPossibleVals(list: List[Cell], whereToIgnore: Set[Int]) = list
      .filter(cell => cell.possibleVals.contains(char) && !whereToIgnore.contains(theFilter(cell)))
      .map(cell => CharToRemove(char, (cell.coordX, cell.coordY)))

    xWings.foldLeft(List[CharToRemove]())((aggregated, current) => {
      val whereToDelete = getWhereToDelete(current)
      val whereToIgnore = getWhereToIgnore(current)
      aggregated ++ toPossibleVals(getColOrRowByIndex(whereToDelete._1), whereToIgnore) ++ toPossibleVals(getColOrRowByIndex(whereToDelete._2), whereToIgnore)
    })
  }
}