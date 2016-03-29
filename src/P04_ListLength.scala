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
  TestRuns.time("Length standard", length(l))
  //time(lengthRec(l))
  TestRuns.time("Length rec", lengthRecTail(l))
  TestRuns.time("Lrngth func", lengthFunc(l))
}
