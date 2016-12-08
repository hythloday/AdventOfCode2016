import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Op
case class Rect(x: Int, y: Int) extends Op
case class RotateRow(y: Int, by: Int) extends Op
case class RotateCol(x: Int, by: Int) extends Op

object Day8 extends App with Advent with JavaTokenParsers {
  val rect = ("rect" ~> decimalNumber <~ "x") ~ decimalNumber ^^ { case x~y => Rect(x.toInt, y.toInt) }
  val rrow = ("rotate row y=" ~> decimalNumber <~ "by") ~ decimalNumber ^^ { case y~by => RotateRow(y.toInt, by.toInt) }
  val rcol = ("rotate column x=" ~> decimalNumber <~ "by") ~ decimalNumber ^^ { case x~by => RotateCol(x.toInt, by.toInt) }

  val (height, width) = (6, 50)
  case class Pixel(x: Int, y: Int)
  val pixels = Set.empty[Pixel]

  val screen = parseAll((rect | rrow | rcol)*, input.mkString("\n")).get.foldLeft(pixels) {
    case (acc, Rect(x, y)) => acc union (for (x1 <- 0 until x; y1 <- 0 until y) yield Pixel(x1, y1)).toSet
    case (acc, RotateRow(y, by)) =>
      val moved = acc.filter(p => p.y == y)
      (acc -- moved) ++ moved.map { case Pixel(x, _) => Pixel((x + by) % width, y) }
    case (acc, RotateCol(x, by)) =>
      val moved = acc.filter(p => p.x == x)
      (acc -- moved) ++ moved.map { case Pixel(_, y) => Pixel(x, (y + by) % height) }
  }

  println(screen.size)

  println((0 until height).map { row =>
    (0 until width).map { col =>
      if (screen contains Pixel(col, row)) '.'
      else ' '
    }.mkString
  }.mkString("\n"))

}
