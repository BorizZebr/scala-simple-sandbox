package src.scala.scalaproblems

/**
  * Created by borisbondarenko on 27.03.16.
  */
object P08_EliminateConsecutiveDuplicates extends App {

  def compress[T](list: List[T]): List[T] =
    list.foldRight(List[T]()) { (elem, acc) =>
      if(acc.isEmpty || acc.head != elem) elem :: acc
      else acc
    }

  val list = List(1, 1, 1, 1, 2, 3, 1, 1, 3, 3, 4, 5, 6, 6, 7, 7)
  println(compress(list))
}
