package src.scala.scalaproblems

/**
  * Created by borisbondarenko on 26.03.16.
  */
object P02_PreLastElement extends App{

  def prelast[T](list: Seq[T]): T = list match{
    case h :: _ :: Nil => h
    case _ :: t => prelast(t)
  }

  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil              => resultList.head
        case _ :: tail        =>
          lastNthR(count - 1,
            if (count > 0) resultList else resultList.tail,
            tail)
      }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }



  println(lastNthRecursive(3, List(1,2,3,4,5,6,7,8,9)))
}
