object Day2 extends App with Advent {

  val keypad = List(
    (0,0) -> 1, (0,1) -> 2, (0,2) -> 3,
    (1,0) -> 4, (1,1) -> 5, (1,2) -> 6,
    (2,0) -> 7, (2,1) -> 8, (2,2) -> 9
  ).toMap

  def transition[T](keypad: Map[(Int, Int), T])(n: T, dir: Char): T = {
    val rev = keypad.map{ case (k, v) => v -> k }
    val (row, col) = rev(n)
    val move = dir match {
      case 'U' => keypad.get(row - 1, col)
      case 'D' => keypad.get(row + 1, col)
      case 'R' => keypad.get(row, col + 1)
      case 'L' => keypad.get(row, col - 1)
    }
    move.getOrElse(n)
  }

  println(input.map(_.toList).foldLeft(List.empty[Int]) { (pin, line) =>
    line.foldLeft(pin.headOption.getOrElse(5))(transition(keypad)) :: pin
  }.reverse.mkString)

  val bathroomKeypad = List(
                                  (0, 2) -> '1',
                   (1, 1) -> '2', (1, 2) -> '3', (1, 3) -> '4',
    (2, 0) -> '5', (2, 1) -> '6', (2, 2) -> '7', (2, 3) -> '8', (2, 4) -> '9',
                   (3, 1) -> 'A', (3, 2) -> 'B', (3, 3) -> 'C',
                                  (4, 2) -> 'D'
  ).toMap
  val rev2 = keypad.map{ case (k, v) => v -> k }

  println(input.map(_.toList).foldLeft(List.empty[Char]) { (pin, line) =>
    line.foldLeft(pin.headOption.getOrElse('5'))(transition(bathroomKeypad)) :: pin
  }.reverse.mkString)

}
