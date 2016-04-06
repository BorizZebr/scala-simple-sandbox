package scalaproblems

import TestRuns._

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P26_27_KCombinationsFromNElements extends App {

  def combinations[T](count: Int, list: List[T]): List[List[T]] = (count, list) match {

    case (1, l) => l.foldLeft(List[List[T]]()){(a, b) => List(b) :: a} reverse
    case (n, l) if n == l.size => l :: Nil
    case (n, h :: t) => combinations(count - 1, t).map(h :: _) ::: combinations(count, t)
  }

  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = {

    require(ns.sum <= list.length)

    ns match{
      case Nil => List(Nil)
      case h :: t => combinations(h, list) flatMap { c =>
        groups(t, list diff c) map { c :: _ }
      }
    }
  }

  val ns = List(3, 1)
  val ls = 1 to 4 toList//names.take(6)

  //println(combinations(2, ls))

  val grps = groups(ns, ls)
  println(grps)
  println(grps.length)
}
