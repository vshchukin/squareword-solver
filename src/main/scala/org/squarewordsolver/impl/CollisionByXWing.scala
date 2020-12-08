package org.squarewordsolver.impl

import org.squarewordsolver._

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
 */
class CollisionByXWing(
                        private val puzzleArea: PuzzleArea
                        ) extends RemoveAdvice {
  private val internalLinesCache = puzzleArea.linesCache

  override def getAdvice: List[CharToRemove] = {
    internalLinesCache.getUnfinishedChars.foldLeft(List[CharToRemove]())((aggr, curr) => xWingCollisionPerChar(curr) ::: aggr)
  }

  private def xWingCollisionPerChar(char: Char) = {
    val cacheOnlyWithChar = puzzleArea.linesCache.newCacheOnlyWithChar(char)

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
      assume(secondPositionsList.isEmpty || secondPositionsList.size == 1, "Illegal state, not a valid squareword")
      if (secondPositionsList.nonEmpty) {
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

    xWings.foldLeft(List[CharToRemove]())((aggregator, current) => {
      val whereToDelete = getWhereToDelete(current)
      val whereToIgnore = getWhereToIgnore(current)
      aggregator ++ toPossibleVals(getColOrRowByIndex(whereToDelete._1), whereToIgnore) ++ toPossibleVals(getColOrRowByIndex(whereToDelete._2), whereToIgnore)
    })
  }
}
