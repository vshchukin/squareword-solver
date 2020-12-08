package org.squarewordsolver.impl

import org.squarewordsolver._

/**
 * If we have next situation
 *
 * X..X
 * . .
 * ..
 * X
 *
 * and X-lower left and X-upper right are on main diagonal (and are the only possible positions of X there)
 * then X-upper left can be eliminated
 * Same thing applies for second-diagonal and for lower-right corner checks
 *
 */

class CollisionByDiagonalSingles(
                                  private val puzzleArea: PuzzleArea
                                  ) extends RemoveAdvice {
  private val linesCache = puzzleArea.linesCache

  override def getAdvice: List[CharToRemove] = {
    linesCache.getUnfinishedChars.map(getCollisionsByChar).foldLeft(List[CharToRemove]())(_ ::: _)
  }

  private def getCollisionsByChar(c: Char): List[CharToRemove] = {
    getCollisionByDiagonal(linesCache.getMainDiagonal, c) ::: getCollisionByDiagonal(linesCache.getSecondDiagonal, c)
  }

  private def getCollisionByDiagonal(diagonal: List[Cell], c: Char): List[CharToRemove] = {
    val possibilities = diagonal.filter(_.possibleVals.contains(c))
    val isThisCharInteresting = possibilities.size == 2
    def getCollisions(xx: Int, yy: Int) = linesCache.at(xx, yy) match {
      case Cell(_, _, _, possibles) if (possibles.contains(c)) => List(CharToRemove(c, (xx, yy)))
      case _ => List[CharToRemove]()
    }
    if (isThisCharInteresting) {
      val (firstCell, secondCell) = (possibilities.head, possibilities(1))
      getCollisions(firstCell.coordX, secondCell.coordY) ::: getCollisions(firstCell.coordY, secondCell.coordX)
    } else List[CharToRemove]()
  }
}
