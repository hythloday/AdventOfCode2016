import math.abs
import breeze.linalg._
import scala.util.parsing.combinator.JavaTokenParsers

object Day1 extends App with Advent with JavaTokenParsers {

  implicit class Rich2dDenseVector(val dv: DenseVector[Int]) extends AnyVal {
    def len: Int = abs(dv(0)) + abs(dv(1))
    def normz: DenseVector[Int] = dv / len
  }

  val right = DenseMatrix((0,-1), (1,0))
  val left = DenseMatrix((0,1), (-1,0))

  val turnLeft = "L" ~> this.decimalNumber <~ ",".? ^^ { digit => left -> digit.toInt}
  val turnRight = "R" ~> this.decimalNumber <~ ",".? ^^ { digit => right -> digit.toInt}
  val parser = (turnLeft | turnRight) *

  val zero = (DenseVector(0, 0), DenseVector(1, 0))
  val (loc, _) = parseAll(parser, input.mkString).get.foldLeft(zero) {
    case ((pos, dir), (turn, step)) =>
      val dir2 = turn * dir
      val pos2 = dir2 :* step
      pos + pos2 -> dir2
  }

  println(loc.len)

  def tween(a: DenseVector[Int], b: DenseVector[Int]): Seq[DenseVector[Int]] = {
    val d = b - a
    for (t <- 0 until d.len) yield a + d.normz * t
  }

  val locs = parseAll(parser, input.mkString).get.scanLeft(zero) {
    case ((pos, dir), (turn, step)) =>
      val dir2 = turn * dir
      val pos2 = dir2 :* step
      (pos + pos2) -> dir2
  }.map(_._1).sliding(2).flatMap {
    case List(a, b) => tween(a, b)
  }.toSeq

  def firstDuplicate[T](xs: Seq[T], visited: Set[T] = Set.empty[T]): T =
    if (visited contains xs.head) xs.head
    else firstDuplicate(xs.tail, visited + xs.head)

  println(firstDuplicate(locs).len)
}
