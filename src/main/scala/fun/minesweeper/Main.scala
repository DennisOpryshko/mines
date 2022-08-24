package fun.minesweeper

import scala.util.Random

object Main extends App {

  val numberOfMines = 15
  val boardSize = 10

  val time = System.currentTimeMillis()
  val rand = new Random(time)
  val game = Sweeper(rand, boardSize, numberOfMines)

  println(game.generate)

}
