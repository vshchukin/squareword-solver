package org.squarewordsolver

/**
 * Experimental
 */
sealed trait Area {
  val sz: Int
  val a: Vector[Item]
  val err: Option[String] = None
}

case class UnfinishedArea(private val size: Int,
                          private val arr: Vector[Item]) extends Area {
  override val a: Vector[Item] = arr
  override val sz: Int = size
}

case class FinishedArea(private val size: Int,
                        private val arr: Vector[FinalChar]) extends Area {
  override val a: Vector[FinalChar] = arr
  override val sz: Int = size
}

case class ErrorArea(private val size: Int,
                     private val arr: Vector[Item],
                     private val errStr: String) extends Area {
  override val err: Option[String] = Some(errStr)
  override val a: Vector[Item] = arr
  override val sz: Int = size
}

sealed abstract class Item

case class FinalChar(c: Char) extends Item

case class OneOfChars(cs: Set[Char]) extends Item
