package org.squarewordsolver

import scala.collection.mutable

/**
 * Holder for puzzle area information, converts input list of strings of form:
 *
 * STRINGONE
 * STRINGTWO
 * STRINGTHR
 * ANOTH STR
 *
 * to internal representation, provides facilities for main possible values reducing loop
 */
class PuzzleArea(
                  val linesCache: LinesCache
                  ) {
  val dimension: Int = linesCache.dimension

  def regenerateFromPossibleValues: Option[PuzzleArea] = {
    def possibleValsThatCanBeRemoved(cell: Cell, list: List[Cell]): Set[Char] = {
      list.filter(_.isSet).map(_.item).foldLeft(Set[Char]())((aggr, c) => if (cell.possibleVals.contains(c)) aggr + c else aggr)
    }
    val toRemovePossibleVals = linesCache.getAllCells.filter(_.isNotSet).map(cell => {
      val megaListToProcess: List[List[Cell]] = (if (cell.coordX == cell.coordY) linesCache.getMainDiagonal else List[Cell]()) ::
        (if (cell.coordX + cell.coordY + 1 == dimension) linesCache.getSecondDiagonal else List[Cell]()) ::
        linesCache.getColumn(cell.coordX) ::
        linesCache.getRow(cell.coordY) :: Nil

      (cell, megaListToProcess.foldLeft(Set[Char]())(_ ++ possibleValsThatCanBeRemoved(cell, _)))
    })
    val firstMap = toRemovePossibleVals.filter(_._2.nonEmpty).toMap
    val secondMap = linesCache.getAllCells.filter(_.isNotSet).map(_.tryGetFound).filter(_.isDefined).map {
      case Some(cell) => ((cell.coordX, cell.coordY), cell)
      case _ => throw new AssertionError()
    }.toMap

    if (firstMap.isEmpty && secondMap.isEmpty) None else Some(new PuzzleArea(linesCache.newCacheWithChangedCells(firstMap).newCacheWithChangedCells2(secondMap)))
  }

  def regenerateFromPossibleValuesSmart: Option[PuzzleArea] = {
    for {
      constrained <- linesCache.getEverythingConstrained
      constrainedNotSet = constrained.filter(_.isNotSet)
      currUnsetChars = constrainedNotSet.foldLeft(Set[Char]())(_ ++ _.possibleVals)
      currUnset <- currUnsetChars
      cellsWithThisChar = constrainedNotSet.filter(_.possibleVals.contains(currUnset))
      if (cellsWithThisChar.size == 1)
    } {
      // we must update at most one char during each travelling, or else will get errors
      return Some(new PuzzleArea(linesCache.newCacheWithChangedCell(cellsWithThisChar.head.foundWithChar(currUnset))))
    }
    None
  }

  def withoutPossibleVals(list: List[CharToRemove]) = new PuzzleArea(linesCache.newCacheWithChangedCells(list))

  private def toString(lineStart: (Int) => String, eachCell: (Int, Int, Char, Set[Char]) => String, lineEnd: (Int) => String) = {
    val result = new mutable.StringBuilder
    for (j <- 0 until dimension; i <- 0 until dimension) {
      if (0 == i) result append lineStart(j)
      val current = linesCache.at(i, j)
      result append eachCell(i, j, current.item, current.possibleVals)
      if (dimension == i + 1) result append lineEnd(j)
    }

    result.toString()
  }

  override def toString: String = {
    val s1 = "    " + (List.tabulate(dimension)(i => "  %10s  ".format(i))).mkString
    val s2 = toString(
      (j) => "%s:: ".format(j),
      (_, _, c, s) => "[ %10s ]".format((if (c != ' ') c else ("!!%s".format(s.mkString)))),
      (j) => "\n"
    )
    "%s\n%s\n\n".format(s1, s2)
  }

  def toStringSimple: String = linesCache.getRows.map(_.map(_.item).mkString) mkString "\n"

}

object PuzzleArea {
  def convert(initialList: List[String]): LinesCache = {
    require(initialList != null, "elems must not be null")
    require(initialList.count(item => (item != null && item.length == initialList.length)) == initialList.length, "list of strings must form a square")

    val dimension = initialList.size
    val allDistinctChars = initialList(0).toCharArray.toSet // 1st line is always filled with all symbols, tradition for squarewords
    val areaArray = Array.tabulate(dimension, dimension)((i, j) => initialList(j).charAt(i) match {
      case ' ' => Cell(i, j, ' ', allDistinctChars)
      case c if (allDistinctChars.contains(c)) => Cell(i, j, c)
      case _ => throw new AssertionError()
    })
    new LinesCache(areaArray)
  }
}
