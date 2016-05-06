package scalaproblems

import java.lang.Math.log

/**
  * Created by borisbondarenko on 03.05.16.
  */
object P55_63_BinTrees extends App {

  sealed abstract class Tree[+T] {
    def addValue[U >: T](x: U)(implicit ord: U => Ordered[U]): Tree[U]
    def isSymmetric: Boolean
    def isMirrorOf[A](t: Tree[A]): Boolean
    def count: Int
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(level: Int): List[T]
    def layoutBinaryTree: Tree[T] = layoutBinaryTreeInner(1, 1)._1
    protected def layoutBinaryTreeInner(x: Int, y: Int): (Tree[T], Int)
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
    override def isSymmetric: Boolean = true
    override def isMirrorOf[A](t: Tree[A]): Boolean = t == End
    override def addValue[U >: Nothing](x: U)(implicit ordered: U => Ordered[U]): Tree[U] = Node(x)
    override def count: Int = 0
    override def leafCount: Int = 0
    override def leafList: List[Nothing] = Nil
    override def internalList: List[Nothing] = Nil
    override def atLevel(level: Int): List[Nothing] = Nil
    override protected def layoutBinaryTreeInner(x: Int, y: Int): (Tree[Nothing], Int) = (End, x)
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

    override def count: Int = left.count + right.count + 1

    override def leafCount: Int = (left, right) match {
      case (End, End) => 1
      case (l, r) => l.leafCount + r.leafCount
    }

    override def leafList: List[T] = (left, right) match {
      case (End, End) => List(value)
      case (l, r) => left.leafList ::: right.leafList
    }

    override def internalList: List[T] = (left, right) match {
      case (End, End) => Nil
      case (l, r) => value :: left.internalList ::: right.internalList
    }

    override def atLevel(level: Int): List[T] = level match {
      case 0 => Nil
      case 1 => List(value)
      case n => left.atLevel(n - 1) ::: right.atLevel(n - 1)
    }

    override protected def layoutBinaryTreeInner(x: Int, y: Int): (Tree[T], Int) = {
      val (l, xl) = left.layoutBinaryTreeInner(x, y + 1)
      val (r, xr) = right.layoutBinaryTreeInner(xl + 1, y + 1)
      (PositionedNode(value, l, r, xl, y), xr)
    }
  }

  case class PositionedNode[+T](
                                 override val value: T,
                                 override val left: Tree[T],
                                 override val right: Tree[T],
                                 x: Int,
                                 y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](count: Int, el: T): List[Tree[T]] = count match {
      case 0 => List(End)

      case n if n % 2 == 0 =>
        val a = cBalanced((n - 1) / 2, el)
        val b = cBalanced((n - 1) / 2 + 1, el)
        a.flatMap(l => b.flatMap(g => List(Node(el, l, g), Node(el, g, l))))

      case n if n % 2 == 1 =>
        val a = cBalanced(n / 2, el)
        a.flatMap(l => a.map(r => Node(el, l, r)))
    }

    def symmetricBalancedTrees[T](count: Int, el: T): List[Tree[T]] =
      cBalanced(count, el) filter { _.isSymmetric }

    def fromList[T](list: List[T])(implicit ord: T => Ordered[T]): Tree[T] =
      list.foldLeft(End: Tree[T]) { (a, b) => a.addValue(b) }

    def hBalancedWithHeight[T](height: Int, el: T): List[Tree[T]] = height match {
      case 1 => List(Node(el))
      case n if n < 1 => List(End)
      case n =>
        val long = hBalancedWithHeight(n - 1, el)
        val short = hBalancedWithHeight(n - 2, el)
        long.flatMap(a => long.map(b => Node(el, a, b))) :::
          long.flatMap(l => short.flatMap(s => List(Node(el, l, s), Node(el, s, l))))
    }

    def minHBalNodes(height: Int): Int = height match {
      case h if h <= 0 => 0
      case 1           => 1
      case h           => 1 + minHBalNodes(h - 1) + minHBalNodes(h - 2)
    }

    def minHBalHeight(nodes: Int): Int =
      if (nodes == 0) 0
      else (log(nodes) / log(2)).toInt + 1

    def maxHbalHeight(nodes: Int): Int =
      if (nodes == 0) 0
      else nodes / 2 + 1

    def hBalanced[T](nodes: Int, value: T): List[Tree[T]] =
      (for{
        h <- minHBalHeight(nodes) to maxHbalHeight(nodes)
        tree <- hBalancedWithHeight(h, value).filter(_.count == nodes)
      } yield tree).toList

    def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
      def loop(addr: Int): Tree[T] =
        if (addr > nodes) End
        else Node(value, loop(addr * 2), loop(addr * 2 + 1))

      loop(1)
    }
  }


  println(Tree.cBalanced(5, "x"))

  println(Node("a", Node("b"), Node("c")).isSymmetric)

  println(End.addValue("a"))

  println(Tree.fromList(List(3, 2, 1, 5, 8, 2, 10)))

  println(Tree.hBalanced(4, "x"))

  println(Tree.completeBinaryTree(5, "x"))
}
