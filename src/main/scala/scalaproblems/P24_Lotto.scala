package scalaproblems

import TestRuns._
import scala.util.Random
import P20_RemoveNthElement._

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P24_Lotto extends App {

  def lotto(n: Int, m: Int): List[Int] = {

    def loop(acc: List[Int], cnt: Int, rest: List[Int], rnd: Random): List[Int] = cnt match {

      case 0 => acc
      case c =>
        val a = removeAtFunc(rest, rnd.nextInt(rest.length))
        loop(a._2 :: acc, c - 1, a._1, rnd)
    }

    if(n > m || n < 0 || m < 0) throw new NoSuchElementException
    loop(Nil, n, (1 to m).toList, new Random)
  }

  time("Lotto test run", lotto(7, 100))
}
