package src.scala.scalaproblems

import scalaproblems.TestRuns
import TestRuns._

import scalaproblems.TestRuns

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P19_RotateList extends App{

  def rotate[T](ls: List[T], n: Int): List[T] = {

    val l = ls.length
    val i = if (n > 0) math.abs(n % l)
    else l + n % l

    val tuple = ls.splitAt(i)
    tuple._2 ::: tuple._1
  }

  val ls = testIntList
  println(ls)

  println(rotate(ls, -3))
}
