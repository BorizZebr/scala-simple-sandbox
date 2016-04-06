package scalaproblems

import TestRuns._

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P18_SliceList extends App {

  def sliceRec[T](ls: List[T], beg: Int, end: Int): List[T] = {

    def _slice(ls: List[T], res: List[T], count: Int): List[T] = (count, ls) match {
      case (_, Nil) => res.reverse
      case (c, _) if end <= c => res.reverse
      case (c, h :: t) if beg <= c => _slice(t, h :: res, c + 1)
      case (c, _ :: t) => _slice(t, res, c + 1)
    }

    _slice(ls, List(), 0)
  }

  val ls = testIntList
  println(ls.slice(4, 7))
  time("Slice standard", ls.slice(4, 7))

  println(sliceRec(ls, 4, 7))
  time("Slice standard", sliceRec(ls, 4, 7))
}
