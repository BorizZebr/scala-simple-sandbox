package scala.scalaproblems

import scalaproblems.TestRuns
import TestRuns.time

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P04_ListLength extends App {

  def length(list: List[_]) = list.length

  def lengthRec(list: List[_]): Int = list match {
    case Nil => 0
    case _ :: t => 1 + lengthRec(t)
  }

  def lengthRecTail(list: List[_]): Int = {

    def _length(list: List[_], res: Int): Int = list match {

      case Nil => throw new NoSuchElementException
      case h :: Nil => res + 1
      case h :: t => _length(t, res + 1)
    }

    _length(list, 0)
  }

  def lengthFunc(list: List[_]) = list.foldLeft(0){(c, _) => c + 1}

  val l = (1 to 10000).toList

  time("Length standard", length(l))
  //time(lengthRec(l))
  time("Length rec", lengthRecTail(l))
  time("Lrngth func", lengthFunc(l))
}
