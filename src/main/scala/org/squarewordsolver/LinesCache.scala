
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
 * @author MrKeyholder
 */

import scala.collection.mutable.Map

class LinesCache(
                  private val dimension: Int,
                  f: (Int, Int) => Cell
                  ) {
  def this(area: Array[Array[Cell]]) = this (area.size, (i, j) => area(i)(j))
  def this(area: PuzzleArea) = this(area.dimension, (i,j)=>area.at(i,j))

  private def this(linesCache: LinesCache, f: (Int, Int, Cell) => Cell) = this (linesCache.dimension, (i, j) => f(i, j, linesCache.at(i, j)))

  private val theArea = Array.tabulate(dimension, dimension)(f)

  private var rows: Map[Int, List[Cell]] = Map.empty
  private var columns: Map[Int, List[Cell]] = Map.empty

  private val mainDiagonal: List[Cell] = tabulateShortcut(i => theArea(i)(i))
  private val secondDiagonal: List[Cell] = tabulateShortcut(i => theArea(i)(dimension - i - 1))

  def getColumn(i: Int) = {
    if (!columns.contains(i)) {
      columns += (i -> tabulateShortcut(jj => theArea(i)(jj)))
    }
    columns(i)
  }

  def getRow(j: Int) = {
    if (!rows.contains(j)) {
      rows += (j -> tabulateShortcut(ii => theArea(ii)(j)))
    }
    rows(j)
  }

  /**
   * return all rows, then all columns, then 1st and 2nd diagonal
   * basically all elements that should have unique constraint
   */
  def getEverythingConstrained = {
    List.tabulate(2 * dimension + 2)(i => {
      if (i < dimension)
        getRow(i)
      else if (i < dimension * 2)
        getColumn(i - dimension)
      else if (i == 2 * dimension)
        getMainDiagonal
      else
        getSecondDiagonal
    })
  }

  /**
   * return all cells on the board
   */
  def getAllCells = getRows.foldLeft(List[Cell]())(_ ++ _)

  /**
   * get every column
   */
  def getColumns = List.tabulate(dimension)(i => getColumn(i))

  /**
   * get every row
   */
  def getRows = List.tabulate(dimension)(i => getRow(i))

  /**
   * return lines cache but with all possible locations for char c on the board
   * For example:
   * A.....A
   * ...A..A
   * A....A.
   */
  def onlyWithChar(c: Char) = {
    def onlyWithCharFunction(i: Int, j: Int, cell: Cell) = cell.possibleVals match {
      case possVlz if possVlz.contains(c) => new Cell(c, cell.coordX, cell.coordY)
      case _ => Cell.empty(i, j)
    }
    new LinesCache(this, (i, j, cell) => onlyWithCharFunction(i, j, this.at(i, j)))
  }

  def getMainDiagonal = mainDiagonal

  def getSecondDiagonal = secondDiagonal

  def at(i: Int, j: Int) = theArea(i)(j)

  private def tabulateShortcut(f: Int => Cell) = List.tabulate(dimension)(f)

  override def toString = {
    def stringRow(j: Int): String = if (dimension == j) "" else "\n%s%s".format(getRow(j).mkString, stringRow(j + 1))
    stringRow(0)
  }

}

object LinesCache {

  /**
   * map the `big` index from LinesCache#getEverythingConstrained method to normal (i,j) coordinates on puzzle area
    */
  def normalCoordinates(bigIndex: Int, indexInConstrainedSequence: Int, dimension: Int): (Int, Int) = bigIndex match {
    case idx if 0 <= idx && idx < dimension => {
      (indexInConstrainedSequence, idx)
    }
    case idx if dimension <= idx && idx < 2 * dimension => {
      (idx - dimension, indexInConstrainedSequence)
    }
    case idx if idx == 2 * dimension => {
      (indexInConstrainedSequence, indexInConstrainedSequence)
    }
    case _ => {
      (indexInConstrainedSequence, dimension - indexInConstrainedSequence - 1)
    }
  }
}