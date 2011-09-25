package org.squarewordsolver.impl

import collection.mutable.ListBuffer
import org.squarewordsolver.{CharToRemove, PuzzleArea, RemoveAdvice}

class CollisionByDiagonalFriendlyPairs(
                                        private val puzzleArea: PuzzleArea,
                                        private val friendlyPairs: Set[((Char, Char), (Int, Int), (Int, Int))]
                                        ) extends RemoveAdvice {
  private val linesCache = puzzleArea.linesCache
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

      updateRemovableValuesFromSet(linesCache.at(i1, j1).possibleVals -- magicSet, i1, j1)

      updateRemovableValuesFromSet(linesCache.at(i2, j2).possibleVals -- magicSet, i2, j2)

      // triangle searches
      // if
      // AB  *  *
      //  *  *  *
      // ABG * AB
      // and rows with AB are on main diagonal
      // then A and B can be removed from cell ABG because A & B must be
      // already in one of the two AB cells
      updateRemovableValuesFromSet(linesCache.at(i1, j2).possibleVals & magicSet, i1, j2)

      updateRemovableValuesFromSet(linesCache.at(i2, j1).possibleVals & magicSet, i2, j1)
    }

    friendlyPairs.foreach {
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