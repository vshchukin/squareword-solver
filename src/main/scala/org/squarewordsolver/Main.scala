
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

import impl._

/**
 * the main entry point to the application
 *
 * @author MrKeyholder
 */

object Main extends App {

  printf("Hello, %s", "Stanislav")
  println()
  println()


  val a = Array(1, 2, 3)

  // TODO implement import functionality
  val initialData = List(
    "NIKOLAEV",
    "        ",
    "   KIEV ",
    "        ",
    "     LAK",
    "VENA    ",
    "        ",
    " KANEV  "
  )

  def printPuzzle(puzzleArea: PuzzleArea) {

    println("    " + (List.tabulate(puzzleArea.dimension)(i => "  %10s  ".format(i))).mkString)

    println(puzzleArea.toString(
      (j) => "%s:: ".format(j),
      (i, j, c, s) => "[ %10s ]".format((if (c != ' ') c else ("!!%s".format(s.mkString)))),
      (j) => "\n"
    ))

    println()
    println()
  }


  val puzzleArea = new PuzzleArea(initialData)

  def getFriendlyPairs = new FriendlyPairsFinder(puzzleArea).get

  puzzleArea.regenerate()
  while (puzzleArea.regenerateFromPossibleValues || puzzleArea.regenerateFromPossibleValuesSmart || {
    val toRemove = new CollisionByDiagonalFriendlyPairs(puzzleArea, getFriendlyPairs).getAdvice
    toRemove.foreach(puzzleArea.removePossibleVal)
    !toRemove.isEmpty
  } || {
    val toRemove = new CollisionByQuadrateFriendlyPairs(puzzleArea, getFriendlyPairs).getAdvice
    toRemove.foreach(puzzleArea.removePossibleVal)
    !toRemove.isEmpty
  } || {
    val toRemove = new CollisionByXWing(puzzleArea).getAdvice
    toRemove.foreach(puzzleArea.removePossibleVal)
    !toRemove.isEmpty
  }
  ) {}

  if (puzzleArea.isNotFinished)
    println("Sorry guys, could not move further to resolve whole puzzle")
  else println("Success!! Puzzle is finished!")
  printPuzzle(puzzleArea)

}