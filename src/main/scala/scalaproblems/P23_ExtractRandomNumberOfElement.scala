package scalaproblems

import P20_RemoveNthElement._
import scala.util.Random
import scalaproblems.TestRuns._

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P23_ExtractRandomNumberOfElement extends App {

  def randomSelect[T](num: Int, list: List[T]): List[T] = {

    def loop(acc: List[T], cnt: Int, rest: List[T], rnd: Random): List[T] = cnt match {

      case 0 => acc
      case n =>
        val a = removeAtFunc(rest, rnd.nextInt(rest.length))
        loop(a._2 :: acc, cnt - 1, a._1, rnd)
    }

    if(list.size < num) throw new NoSuchElementException

    loop(Nil, num, list, new Random)
  }

  val ls = testIntList
  println(ls)
  time("Random take", randomSelect(5, ls))
}
