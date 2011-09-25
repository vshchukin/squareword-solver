package org

/**
 * Experimental
 */
package object squarewordsolver {

  def itemAt(i: Int, j: Int)(implicit area: Area): Item = area.a(i * area.sz + j)

  def rowAt(i: Int)(implicit area: Area): Seq[Item] = area.a.drop(area.sz * Math.max(i - 1, 0)) take area.sz

  def colAt(j: Int)(implicit area: Area): Seq[Item] = sizeRange(area.sz).map(i => area.a(i + j))

  def mainDiag(i: Int, j: Int)(implicit area: Area): Option[Seq[Item]] = if (i == j)
    Some(mainDiag)
  else None

  def mainDiag(implicit area: Area): Seq[Item] = sizeRange(area.sz).map(i => itemAt(i, i))

  def secondDiag(i: Int, j: Int)(implicit area: Area): Option[Seq[Item]] = if (i + j + 1 == area.sz)
    Some(secondDiag)
  else None

  def secondDiag(implicit area: Area): Seq[Item] = sizeRange(area.sz).map(i => itemAt(i, area.sz - i - 1))

  def sizeRange(sz: Int): Range = 0 until sz

  def removableOptions(implicit area: Area): Seq[(Int, Int, Char)] = ??? //simple remove with existing "found" options

  def removableOptions(seq: Seq[Item], allChars: Set[Char]): Seq[(Char, Int)] = {

    val finishedChars: Set[Char] = seq.collect {
      case FinalChar(c) => c
    }.foldLeft(Set[Char]())(_ + _)

    seq.zipWithIndex.collect {
      case (OneOfChars(set), idx) => (set.intersect(finishedChars), idx)
    }.flatMap {
      case (procSet, idx) if !procSet.isEmpty => procSet zip List.tabulate(seq.size)(_ => idx)
    }
  }
}
