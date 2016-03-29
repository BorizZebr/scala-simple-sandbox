/**
  * Created by borisbondarenko on 29.03.16.
  */
object PrefixMapTest extends App {

  val m = PrefixMap("abc" -> 0, "abd" -> 1, "al" -> 2, "all" -> 3, "xy" -> 4)

  //val m = PrefixMap("ab" -> 2, "ac" -> 3)
  println(m)
//  println(m withPrefix "b")
//
//  m += ("hello" -> 5, "hel" -> 3, "hi" -> 2)
//  println(m)
//
//  m -= "hel"
//
//  val mm = m map { case (k, v) => (k + "!", "x" * v)}
//  println(mm)
}