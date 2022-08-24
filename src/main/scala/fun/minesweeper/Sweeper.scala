package fun.minesweeper

import scala.util.Random

case class Point(x: Int, y: Int)
case class Config(size: Int, mines: IndexedSeq[Point], safe: Point)

abstract class Sweeper (config: Config) {

  private val isMine: Point => Boolean =
    config.mines.contains

  private val calcNumber: Point => Int =
    Sweeper.adjacent(_).count(isMine)

  private val discordify: Point => String = {
    case config.safe    => ":blue_square:"
    case p if isMine(p) => "||:boom:||"
    case p              => calcNumber(p) match {
      case 0 => "||:blue_square:||"
      case 1 => "||:one:||"
      case 2 => "||:two:||"
      case 3 => "||:three:||"
      case 4 => "||:four:||"
      case 5 => "||:five:||"
      case 6 => "||:six:||"
      case 7 => "||:seven:||"
      case 8 => "||:eight:||"
      case _ => "ERROR" // for debug
    }
  }

  def generate: String = {

    val all = for {
      x <- 1 to config.size
      y <- 1 to config.size
    } yield Point(x,y)

    all
      .map(discordify)
      .sliding(config.size, config.size)
      .foldLeft("")((acc, xs) => acc + xs.mkString + "\n")

  }

}

object Sweeper {


  def apply(r: Random, size: Int, bombs: Int): Sweeper = {

    val all = for {
      x <- 1 to size
      y <- 1 to size
    } yield Point(x,y)

    val safe = pickPoint(r, size)
    val excluded = adjacent(safe)
    val shuffled = r.shuffle(all.diff(excluded))

    val conf = Config(size, shuffled.take(bombs), safe)
    new Sweeper(conf) {}
  }

  private def pickPoint(r: Random, size: Int): Point = {
    val x = r.nextInt(size - 1) + 1
    val y = r.nextInt(size - 1) + 1
    Point(x,y)
  }

  private val adjacent: Point => Seq[Point] =
    p => for {
      x <- p.x - 1 to p.x + 1
      y <- p.y - 1 to p.y + 1
    } yield Point(x,y)

}