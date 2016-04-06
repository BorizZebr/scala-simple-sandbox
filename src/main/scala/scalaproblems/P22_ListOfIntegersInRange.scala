package scalaproblems

import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P22_ListOfIntegersInRange extends App {

  def range(lo: Int, hi: Int): List[Int] = (lo to hi).toList

  def rangeRec(lo: Int, hi: Int): List[Int] = {

    @tailrec
    def loop(cnt: Int, acc: List[Int]): List[Int] = {

      if(cnt < lo) acc
      else loop(cnt - 1, cnt :: acc)
    }

    loop(hi, Nil)
  }
}
