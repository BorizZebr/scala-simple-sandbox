import TestRuns._

import scala.annotation.tailrec

/**
  * Created by bbondarenko on 3/27/2016.
  */
object P20_RemoveNthElement extends App {

  def removeAtRec[T](ls: List[T], n: Int): (List[T], T) = {
    @tailrec
    def _remove(ls: List[T], res: List[T], count: Int): (List[T], T) = (count, ls) match {
      case(c, h :: t) if n <= c => (res ::: t, h)
      case(c, h :: t) => _remove(t, h :: res, c + 1)
    }

    _remove(ls, List(), 0)
  }

  def removeAtFunc[T](ls: List[T], n: Int): (List[T], T) = {
    val h = ls take n
    val t = ls drop n
    (h ::: t.tail, t.head)
  }

  val ls = testIntList
  println(ls)

  time("RemoveAt Rec", removeAtRec(ls, 4))

  time("RemoveAt Func", removeAtFunc(ls, 4))
}
