/**
  * Created by borisbondarenko on 27.03.16.
  */
object P09_PackList extends App{

  def pack[T](list: List[T]): List[List[T]] = {

    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  val testList = List(1, 1, 1, 1, 2, 3, 3)
  val packedList = pack(testList)

  println(packedList)
}
