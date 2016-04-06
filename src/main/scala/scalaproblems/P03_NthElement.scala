package src.scala.scalaproblems

import scalaproblems.TestRuns
import TestRuns._

import scalaproblems.TestRuns

/**
  * Created by borisbondarenko on 26.03.16.
  */
object P03_NthElement extends App {

  def nthElement[T](ls : List[T], n: Int): T = (n, ls) match {
    case(0, h :: _) => h
    case(i, h :: t) => nthElement(t, i - 1)
    case(_, Nil) => throw new NoSuchElementException
  }


  time("Nth rec", nthElement(TestRuns.testIntList, 5))
  time("Nth stabdard", TestRuns.testIntList(5))
}
