package src.scala.scalaproblems

/**
  * Created by borisbondarenko on 29.03.16.
  */
object P00_HierarchyTailRecursion extends App {

  case class Hierarchy(info: String, children: List[Hierarchy] = Nil) {

    override def toString: String = {

      def _toStrLevel(str: String, l: Int) = "|--\t" * l + str

      def _toString(acc: List[String], ls: List[(Int, Hierarchy)]): List[String] = ls match {

        case Nil => acc
        case (c, Hierarchy(h, Nil)) :: t =>
          _toString(_toStrLevel(h, c) :: acc, t)
        case (c, Hierarchy(h, child)) :: t =>
          _toString(_toStrLevel(h, c) :: acc, child.map((c + 1, _)) ::: t)
      }

      _toString(List(), List(this).map((0, _))).reverse mkString "\n"
    }
  }

  val testHier = Hierarchy("a", List(
    Hierarchy("b"),
    Hierarchy("c", List(
      Hierarchy("cc"),
      Hierarchy("cd", List(
        Hierarchy("cda"),
        Hierarchy("cdb", List(
          Hierarchy("cdba")
        ))
      )),
      Hierarchy("ce")
    )),
    Hierarchy("d")
  ))

  println(testHier)
}
