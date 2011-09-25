
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

package org.squarewordsolver

/**
 * Takes for input list of strings
 *
 * STRINGONE
 * STRINGTWO
 * STRINGTHR
 * ANOTH STR
 *
 * @author MrKeyholder
 */
class PuzzleArea(
                  private val elems: List[String]
                  ) {
  assert(elems != null)
  assert(elems.filter(item => (item != null && item.length() == elems.length)).length == elems.length)

  val dimension = elems.size
  val area = Array.tabulate(dimension, dimension)((i, j) => new Cell(elems(j).charAt(i), i, j))
  val chars = elems(0).toCharArray.toList // assuming 1st line is always filled with all symbols
  private val linesCache = new LinesCache(area)

  // get cell in terms of A 0, B 4
  def at(x: Char, y: Int) = {
    area(x.toUpper - 'A')(y)
  }

  // get normal, xAxis, yAxis
  def at(x: Int, y: Int) = {
    area(x)(y)
  }

  def toString(lineStart: (Int) => String, eachCell: (Int, Int, Char, Set[Char]) => String, lineEnd: (Int) => String) = {
    val result = new StringBuilder
    for (j <- 0 until dimension; i <- 0 until dimension) {
      if (0 == i) result append lineStart(j)
      val current = at(i, j)
      result append eachCell(i, j, current.item, current.possibleVals)
      if (dimension == i + 1) result append lineEnd(j)
    }

    result.toString()
  }

  // TODO add caching objects later
  def regenerate() {
    // iterate over not already discovered values to update possible values
    for (i <- 0 until dimension; j <- 0 until dimension; if at(i, j).isNotSet) {

      val currentCell = at(i, j)

      // if this run is very-very-first
      // TODO remove from here
      if (currentCell.possibleNotInitialized) {
        currentCell.possibleVals = chars.toSet
      }

      // TODO move to a new method later

      val currentRow = linesCache.getRow(j).filter(_.isSet).map(_.item)

      val currentColumn = linesCache.getColumn(i).filter(_.isSet).map(_.item)

      val mainDiagonal = linesCache.getMainDiagonal.filter(i == j && _.isSet).map(_.item)

      val secondDiagonal = linesCache.getSecondDiagonal.filter(i + j + 1 == dimension && _.isSet).map(_.item)

      currentCell.possibleVals = ((((currentCell.possibleVals -- currentRow) -- currentColumn) -- mainDiagonal) -- secondDiagonal)
    }
  }

  def regenerateFromPossibleValues = {
    def updatePossibleValues(cell: Cell, list: List[Cell]) {
      list.filter(_.isSet).map(_.item).foreach(c => {
        if (cell.possibleVals.contains(c)) cell.removePossibleVal(c)
      })
    }
    linesCache.getAllCells.filter(_.isNotSet).foreach(cell => {
      updatePossibleValues(cell, linesCache.getColumn(cell.coordX))
      updatePossibleValues(cell, linesCache.getRow(cell.coordY))
      if (cell.coordX == cell.coordY)
        updatePossibleValues(cell, linesCache.getMainDiagonal)
      if (cell.coordX + cell.coordY + 1 == dimension)
        updatePossibleValues(cell, linesCache.getSecondDiagonal)
    })

    !linesCache.getAllCells.filter(_.isNotSet).map(_.tryToSetFound()).filter(elem => elem).isEmpty
  }

  def regenerateFromPossibleValuesSmart: Boolean = {
    for {
      constrained <- linesCache.getEverythingConstrained
      constrainedNotSet = constrained.filter(_.isNotSet)
      currUnsetChars = constrainedNotSet.foldLeft(Set[Char]())(_ ++ _.possibleVals)
      currUnset <- currUnsetChars
      cellsWithThisChar = constrainedNotSet.filter(_.possibleVals.contains(currUnset))
      if (cellsWithThisChar.size == 1)
    } {
      // we must update at most one char during each travelling, or else will get errors
      cellsWithThisChar.head.setFound(currUnset)
      return true
    }
    false
  }


  def getUnfinishedChars = linesCache.getEverythingConstrained.foldLeft(Set[Char]())((aggregated, curr) => aggregated ++ curr.foldLeft(Set[Char]())(_ ++ _.possibleVals))

  def removePossibleVal(c: CharToRemove) {
    area(c.fromCoordinate._1)(c.fromCoordinate._2).removePossibleVal(c.theChar)
  }

  // whether this puzzle is finished or not
  def isFinished = linesCache.getEverythingConstrained.forall(_.forall(_.isSet))

  def isNotFinished = !isFinished

}