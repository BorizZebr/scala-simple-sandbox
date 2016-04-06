package src.scala.scalaproblems

import scalaproblems.TestRuns
import TestRuns._

import scala.annotation.tailrec
import scalaproblems.TestRuns

/**
  * Created by borisbondarenko on 26.03.16.
  */
object P01_LastListElement extends App{

  @tailrec
  def lastRecursive[T](list: List[T]): T = list match {

    case h :: Nil => h
    case _ :: t => lastRecursive(t)
    case _ => throw new NoSuchElementException
  }

  def last[T](list: Seq[T]) = list.last

  def lastFunc[T](list: List[T]): T = list.reduceLeft { (a, b) => b }

  time("Last standard", last(List(1, 2, 3, 4, 5)))
  time("Last rec", lastRecursive(List(1, 2, 3, 4, 5)))
  time("Last func", lastFunc(List(1, 2, 3, 4, 5)))
}
