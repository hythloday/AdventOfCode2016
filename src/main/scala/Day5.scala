import java.security.MessageDigest

object Day5 extends App with Advent {

  val code = input.head.trim

  def md5(s: String): Array[Byte] = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  def nats(i: Long): Stream[Long] = i #:: nats(i + 1)

  object InterestingHash {
    def unapply(bytes: Array[Byte]): Option[Array[Byte]] = {
      if (bytes(0) == 0 && bytes(1) == 0 && (bytes(2) & 0xF0) == 0) Some(bytes)
      else None
    }
  }

  def hashes: Stream[String] = nats(0L).map { i =>
    md5(s"$code$i")
  }.filter {
    case InterestingHash(hash) => true
    case _ => false
  }.map {
    _.map("%02X" format _).mkString
  }

  hashes.map(_(5)).take(8).foreach(print)

  val letters = hashes.scanLeft(Map.empty[Int, Char]) { case (pw, hash) =>
    val (pos, chr) = (hash(5).getNumericValue, hash(6))
    if (pw.isDefinedAt(pos)) pw
    else if (pos > 7) pw
    else { pw + (pos -> chr) }
  }.map { last =>
    last.keySet.toSeq.sorted.map(last).mkString
  }

  letters.takeWhile(x => x.length < 9).foreach(println)
}
