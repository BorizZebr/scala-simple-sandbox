package scalaproblems

import scala.collection.mutable

/**
  * Created by borisbondarenko on 25.06.16.
  */
object A00_Substrings extends App {

  def subsetOfKImperative(str: String, k: Int): Seq[String] = {

    var acc: List[String] = Nil
    val charMap: mutable.Map[Char, Int] = mutable.Map()
    var i, j: Int = 0
    def origCount = charMap.count { case (_, v) => v > 0 }

    while (i < str.length && j <= str.length) {

      if (j == str.length && origCount == k) {
        acc = str.substring(i, j) :: acc
        j = j + 1
      }
      else if (origCount > k) {
        // шагаем головой
        val c = str(i)
        charMap.update(c, charMap.get(c).get - 1)
        i = i + 1
      } else {
        // шагаем хвостом
        val c = str(j)
        val value = charMap.getOrElse(c, 0)
        if (value > 0) charMap.update(c, value + 1)
        else {
          if (origCount == k) acc = str.substring(i, j) :: acc
          charMap.update(c, 1)
        }
        j = j + 1
      }
    }

    acc reverse
  }

  val str = "ABBDACBADDDAAACBBACDDABAAAABAADCCABBADDACADDACBBBAADA"
  val res = subsetOfKImperative(str, 3)
  println(res)
  println(res.sortBy(_.length).last)
}
