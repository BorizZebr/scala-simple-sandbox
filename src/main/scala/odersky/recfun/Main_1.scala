package odersky.recfun

object Main_1 {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if(c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(co: Int, ls: List[Char]): Boolean = (co, ls) match {

      case (c, Nil) => c == 0
      case (0, ')' :: _) => false
      case (c, ')' :: t) => loop(c - 1, t)
      case (c, '(' :: t) => loop(c + 1, t)
      case (c, _ :: t) => loop(c, t)
    }

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {

    case (0, _) => 1
    case (_, Nil) => 0
    case (m, _) if m < 0 => 0
    case (m, h :: t) => countChange(m - h, h :: t) + countChange(m, t)
  }
}
