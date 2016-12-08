import scala.util.parsing.combinator.JavaTokenParsers

object Day7 extends App with Advent with JavaTokenParsers {
  def abba(s: String) = s.sliding(4).map(_.toList).exists {
    case List(a, b, c, d) if a == d && b == c && a != b => true
    case _ => false
  }
  def bab(s: String) = s.sliding(3).map(_.toList).collect {
    case List(a, b, c) if a == c && a != b => (b, a)
  }
  def aba(s: String) = s.sliding(3).map(_.toList).collect {
    case List(a, b, c) if a == c && a != b => (a, b)
  }
  object TLS {
    def unapply(nets: (List[String], List[String])): Boolean = {
      val (hnets, snets) = nets
      hnets.exists(abba) && !snets.exists(abba)
    }
  }
  object SSL {
    def unapply(nets: (List[String], List[String])): Boolean = {
      val (hnets, snets) = nets
      val abas = snets.flatMap(aba).toSet
      val babs = hnets.flatMap(bab).toSet
      (abas intersect babs).nonEmpty
    }
  }
  val address = (ident | ("[" ~> ident <~ "]")).* ^^ { parts =>
    parts.grouped(2).map(_.toArray).toArray.transpose.map(_.toList) match {
      case Array(good, bad) => (good, bad)
      case Array(good) => (good, List.empty)
      case _ => println(parts); (List.empty, List.empty)
    }
  }
  println(input.map(parseAll(address, _).get).count {
    case TLS() => true
    case _ => false
  })
  println(input.map(parseAll(address, _).get).count {
    case SSL() => true
    case _ => false
  })
}
