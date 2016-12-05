import scala.util.parsing.combinator.JavaTokenParsers

object Day4 extends App with Advent with JavaTokenParsers {

  def rle[T](xs: Seq[T]): List[(T, Int)] = xs.foldLeft(List.empty[(T, Int)]) {
    case ((prevX, count) :: tail, x) if x == prevX => (x, count + 1) :: tail
    case (l, x) => (x, 1) :: l
  }

  val line = repsep(ident,  "-") ~ ("-" ~> decimalNumber) ~ ("[" ~> ident <~ "]") ^^ {
    case name~number~checksum => (name, number.toInt, checksum)
  }

  object ChecksumSorting extends Ordering[(Char, Int)] {
    override def compare(x: (Char, Int), y: (Char, Int)): Int = (x, y) match {
      case ((_, ix), (_, iy)) if ix != iy => iy compare ix
      case ((cx, _), (cy, _)) => cx compare cy
    }
  }

  println(parseAll(line *, input.mkString("\n")).get.filter { case (name, number, checksum) =>
    rle(name.flatMap(_.toList).sorted).sorted(ChecksumSorting).take(5).map(_._1).mkString == checksum
  }.map(_._2).sum)

  def rot(n: Int, s: String): String = s.toList.map {
    case '-' => ' '
    case c =>
      val a = 'a'.toLong
      (a + ((c.toLong - a + n) % 26)).toChar
  }.mkString

  println(parseAll(line *, input.mkString("\n")).get.filter { case (name, number, checksum) =>
    rle(name.flatMap(_.toList).sorted).sorted(ChecksumSorting).take(5).map(_._1).mkString == checksum
  }.map {
    case (name, number, checksum) => rot(number, name.flatMap(_.toList).mkString) -> number
  }.filter {
    case (name, number) => name contains "north"
  })

}
