1. What is Squareword?
Squareword is a square divided into cells. "Words" are written into cells in particular order.
The goal is to fill in empty cells with letters from first row given set so that in every row,
column and in every of two main diagonals there were no same letters. Think of it as variation of
Sudoku.
For example:

 S L E Z A
 * * * * *
 * * L E S
 * * * * *
 * * * * *

By tradition first row is always fully filled with letters.
`*` sign indicates empty cells that should be filled. Correct solution to this example is following:

 S L E Z A
 A E Z S L
 Z A L E S
 L Z S A E
 E S A L Z

2. TODO list:
- solving of squarewords with more than one correct solution

5. Build/Run requirements:
- JDK 8+
- sbt 0.13.x
