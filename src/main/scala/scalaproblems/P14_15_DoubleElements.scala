package src.scala.scalaproblems

import src.scala.TestRuns
import TestRuns._

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P14_15_DoubleElements extends App {

  def duplicate[T](ls: List[T]): List[T] = duplicate(ls, 2)

  def duplicate[T](ls: List[T], n: Int): List[T] = ls flatMap { List.fill(n)(_)}

  val list = testSimpleList
  time("Duplicate", duplicate(list))
  time("Duplicate 3 times", duplicate(list, 3))
}
