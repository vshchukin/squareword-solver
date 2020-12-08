package org.squarewordsolver

/**
 * from what coordinate should we remove `theChar` as possible char candidate?
 */

case class CharToRemove(
                         theChar: Char,
                         fromCoordinate: (Int, Int)
                       )
