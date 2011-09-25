package org.squarewordsolver

/**
 * immutable class to get cells in easy to use data structures
 */
class LinesCache(
                  val dimension: Int,
                  f: (Int, Int) => Cell
                  ) {

  def this(area: Array[Array[Cell]]) = this (area.size, (i, j) => area(i)(j))

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

  def getUnfinishedChars = getEverythingConstrained.foldLeft(Set[Char]())(_ ++ _.foldLeft(Set[Char]())(_ ++ _.possibleVals))

  /**
   * main loop supporter
   */
  def newCacheWithChangedCell(newCell: Cell) = new LinesCache(dimension, (x, y) => if (x == newCell.coordX && y == newCell.coordY) newCell else at(x, y))

  def newCacheWithChangedCells(mapOfReducedCells: Map[Cell, Set[Char]]) = new LinesCache(dimension, (x, y) => {
    val atXY = at(x, y)
    mapOfReducedCells.get(atXY) match {
      case Some(charsToRemove) => atXY.withoutPossibleChars(charsToRemove)
      case None => atXY
    }
  })

  def newCacheWithChangedCells2(mapOfReducedCells: Map[(Int, Int), Cell]) = new LinesCache(dimension, (x, y) => {
    val atXY = at(x, y)
    mapOfReducedCells.get((atXY.coordX, atXY.coordY)) match {
      case Some(cell) => cell
      case None => atXY
    }
  })

  def newCacheWithChangedCells(list: List[CharToRemove]) = new LinesCache(dimension, (x, y) => {
    val toRemoveCurr = list.filter(chTR => chTR.fromCoordinate._1 == x && chTR.fromCoordinate._2 == y)
    val atXY = at(x, y)
    if (toRemoveCurr.isEmpty) atXY else atXY.withoutPossibleChars(toRemoveCurr.map(_.theChar).toSet)
  })

  /**
   * return lines cache but with all possible locations for char c on the board
   * For example:
   * A.....A
   * ...A..A
   * A....A.
   */
  def newCacheOnlyWithChar(c: Char) = {
    def onlyWithCharFunction(i: Int, j: Int, cell: Cell) = cell.possibleVals match {
      case possVlz if possVlz.contains(c) => Cell(cell.coordX, cell.coordY, c)
      case _ => Cell.empty(i, j)
    }
    new LinesCache(this, (i, j, cell) => onlyWithCharFunction(i, j, this.at(i, j)))
  }

  def getMainDiagonal = mainDiagonal

  def getSecondDiagonal = secondDiagonal

  def at(i: Int, j: Int) = theArea(i)(j)

  private def tabulateShortcut(f: Int => Cell) = List.tabulate(dimension)(f)

  override def equals(obj: Any) = classOf[LinesCache] == obj.getClass && getAllCells == obj.asInstanceOf[LinesCache].getAllCells

  override def hashCode = getAllCells.hashCode()

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