/**
  * Created by borisbondarenko on 27.03.16.
  */
object P07_FlattenNestedList extends App{

  def flatten(ls: List[Any]): List[Any] = ls flatten {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
}
