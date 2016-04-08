package scalaproblems

/**
  * Created by borisbondarenko on 07.04.16.
  */
object P28_SortingListOfSublists extends App {

  def lsort[T](ls: List[List[T]]): List[List[T]] = ls.sortWith((a, b) => a.length > b.length)

  def lsortFreq[T](ls: List[List[T]]): List[List[T]] = {
//    val freqs = ls.foldLeft(Map[Int, Int]() withDefaultValue 0) { (a, b) =>
//      a.updated(b.length, a(b.length) + 1)
//    }
    val freqs = ls.groupBy(_.length).map(e => e._1 -> e._2.length)
    ls.sortWith((e1, e2) => freqs(e1.length) > freqs(e2.length))
  }

  val a = lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  println(a)
}
