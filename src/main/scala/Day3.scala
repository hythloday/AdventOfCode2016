object Day3 extends App with Advent {

  def triangle: PartialFunction[Array[Int], Boolean] = {
    case Array(a, b, c) if a + b > c => true
    case _ => false
  }

  println(input.map(_.trim.split("[ ]+").map(_.toInt)).map(_.sorted).count(triangle))
  println(input.map(_.trim.split("[ ]+").map(_.toInt)).toArray.transpose.flatten.grouped(3).map(_.sorted).count(triangle))
}
