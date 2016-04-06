package scalaproblems

import TestRuns._
import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P21_InsertElementAtPosition extends App {

  def insertAtPositionFunc[T](elem: T, pos: Int, list: List[T]): List[T] = {

    val pre = list take pos
    val post = list drop pos
    pre ::: elem :: post
  }

  def insertAtPositionRec[T](elem: T, pos: Int, list: List[T]): List[T] = {

    @tailrec
    def loop(counter: Int, acc: List[T], ls: List[T]): List[T] = (counter, ls) match {

      case (n, h :: t) if n == pos => t ::: h :: elem :: acc
      case (n, h :: t) => loop(n + 1, h :: acc, t)
    }

    loop(0, List(), list) reverse
  }

  def insertAtPositionElegant[T](elem: T, pos: Int, list: List[T]): List[T] = list.splitAt(pos) match {
    case (pre, post) => pre ::: elem :: post
  }

  val ls = testIntList
  println(ls)
  time("Insert func", insertAtPositionFunc(100500, 5, ls))
  time("Insert rec", insertAtPositionRec(100500, 5, ls))
  time("Insert elegant", insertAtPositionElegant(100500, 5, ls))
}
