
object Day6 extends App with Advent {
  println(input.map(_.toArray).toArray.transpose.map(_.toSeq.sorted).map(rle).map(_.sortBy(_._2).reverse.head._1).mkString)
  println(input.map(_.toArray).toArray.transpose.map(_.toSeq.sorted).map(rle).map(_.sortBy(_._2).head._1).mkString)

}
