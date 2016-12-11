import scala.util.parsing.combinator.JavaTokenParsers

object Day9 extends App with Advent with JavaTokenParsers {

  val text = ident ^^ (_.length.toLong)
  val marker = ("(" ~> decimalNumber <~ "x") ~ (decimalNumber <~ ")")
  val p1 = marker >> { case take~reps => s".{$take}".r ^^ { s => reps.toLong * s.length } }

  println(parseAll((p1 | text)*, input.head).get.sum)

  val p2: Parser[Long] = marker >> { case take~reps =>
    s".{$take}".r ^^ { s: String => parseAll((p2 | text)*, s).get.sum * reps.toLong } }

  println(parseAll((p2 | text)*, input.head).get.sum)
}
