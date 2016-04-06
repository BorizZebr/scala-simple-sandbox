package src.scala.scalaproblems

import scalaproblems.TestRuns
import TestRuns._

import scalaproblems.TestRuns

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P16_DropNthElement extends App {

  def drop[T](n: Int, ls: List[T]): List[T] = {

    def _drop(c: Int, ls: List[T], res: List[T]): List[T] = (c, ls) match {

      case (_, Nil) => res.reverse
      case (0, _ :: t) => _drop(n, t, res)
      case (_, h :: t) => _drop(c - 1, t, h :: res)
    }

    _drop(n, ls, List())
  }

  def dropFunc[T](n: Int, ls: List[T]): List[T] =
    ls.zipWithIndex filter {e => (e._2 + 1) % n != 0} map {_._1}

  def dropGrouped[T](n: Int, ls: List[T]): List[T] =
    ls.grouped(n) flatMap {e => e.take(n - 1)} toList

  val ls = 1 to 10000 toList//testIntList

  time("Drop func", dropFunc(3, ls))
  time("Drop rec", drop(3, ls))
  time("Drop grouped", dropGrouped(3, ls))
}
