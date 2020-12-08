package org.squarewordsolver.impl

import org.squarewordsolver.{CharToRemove, PuzzleArea, RemoveAdvice}

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
  private val linesCache = puzzleArea.linesCache

  override def getAdvice: List[CharToRemove] = {
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

    val charsToRemove = for {
      correctCandidate <- correctQuadrateCandidates
      currSetOfPairs = {
        Set(correctCandidate._1._1, correctCandidate._1._2)
      }

      toList = correctCandidate._2.toList
      fourthCornerCoords = (toList(2)._1, toList(1)._2)
      fourthCornerCell = linesCache.at(fourthCornerCoords._1, fourthCornerCoords._2)
      if (fourthCornerCell.isNotSet)
      charsToRemove = currSetOfPairs & fourthCornerCell.possibleVals
      if charsToRemove.nonEmpty
    }
    yield CharToRemove(charsToRemove.head, fourthCornerCoords)

    println(charsToRemove)

    charsToRemove.toList
  }
}
