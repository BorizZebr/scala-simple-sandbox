import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 27.03.16.
  */
object CheckMyself extends App{

  // get last kth element
  def kth[T](list: List[T], n: Int): T = {

    @tailrec
    def kthInternal(list: List[T], reslist: List[T], count: Int): T = list match {

      case Nil => reslist.head
      case h :: t => kthInternal(t,
        if (count > 0) reslist else reslist.tail, count - 1)
    }

    kthInternal(list, list, n)
  }

  // flatten nested
  def flattenNested[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case (h : List[T]) :: t => flattenNested(h) ::: flattenNested(t)
    case h :: t => h :: flattenNested(t)
  }


  val testList = List(1, 2, 3, 4, 5, 6)
  println(kth(testList, 2))

  println(flattenNested(List(List(1, 1), 2, List(3, List(5, 8)))))
}