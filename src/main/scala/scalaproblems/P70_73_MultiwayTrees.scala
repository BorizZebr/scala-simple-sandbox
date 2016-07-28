package scalaproblems

import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 25.07.16.
  */
object P70_73_MultiwayTrees extends App {

  object MTree {
    def apply(str: String): MTree[Char] = {

      def splitChildStrings(input: List[Char]): List[String] = {
        @tailrec
        def loop(
            accList: List[String],
            accItem: List[Char],
            rest: List[Char],
            nesting: Int): List[String] = (nesting, rest) match {

          case (0, rst) if accItem.nonEmpty =>
            loop(accItem.reverse.mkString :: accList, Nil, rst, 0)
          case (_, Nil) =>
            accList
          case (n, h :: t) =>
            val nstng = if (h == '^') n - 1 else n + 1
            loop(accList, h :: accItem, t, nstng)
        }
        loop(Nil, Nil, input, 0) reverse
      }

      val s = str.toList
      MTree(s.head, splitChildStrings(s.tail).map(MTree(_)))
    }
  }

  case class MTree[+T](value: T, children: List[MTree[T]] = Nil) {

    def this(value: T) = this(value, List())

    def nodeCont: Int = {
      @tailrec
      def loop(acc: Int, rest: List[MTree[T]]): Int =
        if (rest.isEmpty) acc
        else loop(acc + rest.size, rest.flatMap(_.children))

      loop(1, children)
    }

    def internalPathLength: Int = {
      @tailrec
      def loop(acc: Int, rest: List[MTree[T]], depth: Int): Int =
        if (rest.isEmpty) acc
        else loop(acc + depth * rest.size, rest.flatMap(_.children), depth + 1)

      loop(0, children, 1)
    }

    def lispyTree: String =
      if (children == Nil) value.toString
      else s"(${value.toString} ${children.map(_.lispyTree).mkString(" ")})"

    def postorder: List[T] = children.flatMap(_.postorder) ::: List(value)

    override def toString: String = value.toString + children.map(_.toString + "^").mkString
  }


  val tree = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
  val treeFromString = MTree("afg^^c^bd^e^^")

  assert(tree.nodeCont == 7)
  assert(tree.toString == "afg^^c^bd^e^^")
  assert(tree.equals(treeFromString))
  assert(treeFromString.internalPathLength == 9)
  assert(treeFromString.postorder == List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
  assert(treeFromString.lispyTree == "(a (f g) c (b d e))")

}
