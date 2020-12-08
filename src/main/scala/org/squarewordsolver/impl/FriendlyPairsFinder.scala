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
 */

class FriendlyPairsFinder(
                           private val puzzleArea: PuzzleArea
                           ) {
  private val dimension = puzzleArea.dimension
  private val linesCache = puzzleArea.linesCache

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
      constrainedSeq.count(_.possibleVals.contains(c)) match {
        case 2 => Set(c)
        case _ => Set()
      }
    }

    val onlyTwoChars = possiblesSet.foldLeft(Set[Char]())(_ ++ getIfOnlyTwo(_))


    val distinctPairsOfOnlyTwoChars = onlyTwoChars.foldLeft(Set[(Char, Char)]())((sum, next) =>
      sum ++ onlyTwoChars.filter(_ != next).map(el => if (!sum.contains((next, el))) (el, next) else (next, el)))

    val onlies = new ListBuffer[((Char, Char), (Int, Int), (Int, Int))]()
    distinctPairsOfOnlyTwoChars.foreach {
      case (first, second) => {
        def possibleValsContainsFirstAndSecond(c: Cell): Boolean = Set(first, second).forall(c.possibleVals.contains)
        val firstIdx = constrainedSeq.indexWhere(possibleValsContainsFirstAndSecond)
        val secondIdx = constrainedSeq.indexWhere(possibleValsContainsFirstAndSecond, firstIdx + 1)
        if (-1 != firstIdx && -1 != secondIdx) {
          onlies += (((first, second), LinesCache.normalCoordinates(index, firstIdx, dimension), LinesCache.normalCoordinates(index, secondIdx, dimension)))
        }
      }
    }
    onlies.toSet
  }
}
