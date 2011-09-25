package org.squarewordsolver

import impl._

/**
 * the main entry point to the application
 */

object Main{

  private def mainLoop(puzzleArea:PuzzleArea): Option[PuzzleArea] = {
    puzzleArea.regenerateFromPossibleValues match {
      case Some(pa) =>{
        return Some(pa)
      }
      case None =>
    }

    puzzleArea.regenerateFromPossibleValuesSmart match {
      case Some(pa) => {
        return Some(pa)
      }
      case None =>
    }

    val diagSingles = new CollisionByDiagonalSingles(puzzleArea).getAdvice
    if (!diagSingles.isEmpty) {
      return Some(puzzleArea.withoutPossibleVals(diagSingles))
    }

    val diagFP = new CollisionByDiagonalFriendlyPairs(puzzleArea, new FriendlyPairsFinder(puzzleArea).get).getAdvice
    if (!diagFP.isEmpty) {
      return Some(puzzleArea.withoutPossibleVals(diagFP))
    }

    val quadro = new CollisionByQuadrateFriendlyPairs(puzzleArea, new FriendlyPairsFinder(puzzleArea).get).getAdvice
    if (!quadro.isEmpty) {
      return Some(puzzleArea.withoutPossibleVals(quadro))
    }

    val xWing = new CollisionByXWing(puzzleArea).getAdvice
    if (!xWing.isEmpty) {
      return Some(puzzleArea.withoutPossibleVals(xWing))
    }

    None
  }

  def mainSolving(prevHistory: List[PuzzleArea]): List[PuzzleArea] = {
    val prevArea = prevHistory.head
    val currAreaOption = mainLoop(prevArea)
    if (!(currAreaOption.isDefined && prevHistory.size < 300))
      prevHistory
    else
      mainSolving(currAreaOption.get :: prevHistory)
  }

  def generalResultOf(l:List[String]) = mainSolving(List(new PuzzleArea(PuzzleArea convert l))).head
}