package scalaproblems

/**
  * Created by borisbondarenko on 03.05.16.
  */
object P55_BinTrees extends App {

  sealed abstract class Tree[+T]

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](count: Int, el: T): List[Node[T]] = count match {
      
    }
  }

}
