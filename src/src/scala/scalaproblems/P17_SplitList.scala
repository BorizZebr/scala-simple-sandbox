package src.scala.scalaproblems

import src.scala.TestRuns
import TestRuns._

import scala.annotation.tailrec

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P17_SplitList extends App {

  def split[T](ls: List[T], n: Int): (List[T], List[T]) = ls.splitAt(n)

  def splitRec[T](ls: List[T], n: Int): (List[T], List[T]) = {

    @tailrec
    def _split[A](ls: List[A], res: List[A], n: Int): (List[A], List[A]) = (n, ls) match {
      case(_, Nil) => (res.reverse, Nil)
      case(0, list) => (res.reverse, list)
      case(i, h :: t) => _split(t, h :: res, i - 1)
    }

    _split(ls, List(), n)
  }

  val ls = testIntList

  println(ls)
  time("Split rec", splitRec(ls, 2))
  println(splitRec(ls, 2))

  time("Standard split", split(ls, 2))
  println(split(ls, 2))

}
