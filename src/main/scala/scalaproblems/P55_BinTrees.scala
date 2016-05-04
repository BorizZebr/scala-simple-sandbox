package scalaproblems

/**
  * Created by borisbondarenko on 03.05.16.
  */
object P55_BinTrees extends App {

  sealed abstract class Tree[+T] {

    def addValue[U >: T](x: U)(implicit ord: U => Ordered[U]): Tree[U]

    def isSymmetric: Boolean

    def isMirrorOf[A](t: Tree[A]): Boolean
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString =
      "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isSymmetric: Boolean = left.isMirrorOf(right)

    override def isMirrorOf[A](t: Tree[A]): Boolean = t match {
      case Node(_, nl, nr) => nl.isMirrorOf(right) && nr.isMirrorOf(left)
      case _ => false
    }

    override def addValue[U >: T](x: U)(implicit ord: (U) => Ordered[U]): Tree[U] =
      if (x > value) Node(value, left, right.addValue(x))
      else Node(value, left.addValue(x), right)
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isSymmetric: Boolean = true

    override def isMirrorOf[A](t: Tree[A]): Boolean = t == End

    override def addValue[U >: Nothing](x: U)(implicit ordered: U => Ordered[U]): Tree[U] =
      Node(x)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](count: Int, el: T): List[Tree[T]] = count match {
      case n if n == 0 => List(End)

      case n if n % 2 == 0 =>
        val a = cBalanced((n - 1) / 2, el)
        val b = cBalanced((n - 1) / 2 + 1, el)
        a.flatMap(l => b.flatMap(g => List(Node(el, l, g), Node(el, g, l))))

      case n if n % 2 == 1 =>
        val a = cBalanced(n / 2, el)
        a.flatMap(l => a.map(r => Node(el, l, r)))
    }
  }


  println(Tree.cBalanced(4, "x"))

  println(Node("a", Node("b"), Node("c")).isSymmetric)
}
