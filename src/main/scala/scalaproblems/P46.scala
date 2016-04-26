package scalaproblems

/**
  * Created by borisbondarenko on 25.04.16.
  */
object P46 extends App {

  implicit class BoolOps(a: Boolean) {
    def and (b: Boolean) = a && b
    def or  (b: Boolean) = a || b
    def nand(b: Boolean) = not(a and b)
    def nor (b: Boolean) = not(a or b)
    def equ (b: Boolean) = (a and b) or (not(a) and not(b))
    def xor (b: Boolean) = not(a equ b)
    def impl(b: Boolean) = not(a) or b
  }

  def not(a: Boolean) = !a

  def table2(f: (Boolean, Boolean) => Boolean) : Unit = {
    val bools = Set(true, false)
    println("A\t\tB\t\tRes")
    for {
      a <- bools
      b <- bools
    } println(s"$a\t$b\t" + f(a,b))
  }

  def gray(c: Int): List[String] = {
    val digits = Set(0, 1)

    def loop(rest: Int) : List[List[Int]] = {
      if (rest == 1) List(List(0), List(1))
      else for {
        l <- loop(rest - 1)
        n <- digits
      } yield n :: l
    }

    loop(c) map { _.reverse.mkString }
  }


  table2((a: Boolean, b: Boolean) => a and (a or not(b)))

  println(gray(5))
}
