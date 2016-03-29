import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 27.03.16.
  */
object P05_ReverseList extends App{

  def revRec[T](list: List[T]): List[T] = list match {

    case h :: t => revRec(t) ::: List(h)
    case Nil => throw new NoSuchElementException
  }

  def revTailRec[T](list: List[T]) = {

    def revInternal(list: List[T], result: List[T]): List[T] = list match{

      case Nil => result
      case h :: t => revInternal(t, h :: result)
    }
    revInternal(list, Nil)
  }

  def revFunc[T](list: List[T]) = list.foldLeft(List[T]()){ (a, b) => b :: a }
}
