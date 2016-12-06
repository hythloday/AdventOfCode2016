/**
  * Created by james on 04/12/2016.
  */
trait Advent {

  def input: List[String] = io.Source.fromFile(s"${getClass.getName.toLowerCase.init}.txt").getLines().toList

  def rle[T](xs: Seq[T]): List[(T, Int)] = xs.foldLeft(List.empty[(T, Int)]) {
    case ((prevX, count) :: tail, x) if x == prevX => (x, count + 1) :: tail
    case (l, x) => (x, 1) :: l
  }


}
