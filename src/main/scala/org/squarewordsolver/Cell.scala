package org.squarewordsolver

/**
 * single immutable item in Squareword field
 */
case class Cell(
                 coordX: Int,
                 coordY: Int,
                 item: Char,
                 possibleVals: Set[Char] = Set[Char]()
                 ) {
  /**
   * get found cell from this one
   * @param c the found char
   * @throws IllegalArgumentException if c is not possible char for this cell
   * @return found cell
   */
  def foundWithChar(c: Char) = {
    require(possibleVals.contains(c), "char %s is not in possible values for this cell")
    Cell(coordX, coordY, c)
  }

  /**
   * @return Some Cell if current cell can be marked as found, None otherwise
   */
  def tryGetFound = if (possibleVals.size == 1) Some(Cell(coordX, coordY, possibleVals.head)) else None

  /**
   * get new cell that is equal to current cell with reduced possible values by param char
   * @param possibleChar to reduce
   * @throws IllegalArgumentException if possibleChar is not possible char for this cell
   * @return reduced Cell
   */
  def withoutPossibleChar(possibleChar: Char) = {
    require(possibleVals.contains(possibleChar), "%s does not contain char %s".format(this, possibleChar))
    Cell(coordX, coordY, item, possibleVals - possibleChar)
  }

  def withoutPossibleChars(possibleChars: Set[Char]) = Cell(coordX, coordY, item, possibleVals -- possibleChars)

  def isSet = (' ' != item)

  def isNotSet = !isSet

  def is(i: Char) = (i == item)
}

object Cell {
  def empty(coordX: Int, coordY: Int) = Cell(coordX, coordY, ' ')
}