/**
  * Created by james on 04/12/2016.
  */
trait Advent {

  def input: List[String] = io.Source.fromFile(s"${getClass.getName.toLowerCase.init}.txt").getLines().toList
}
