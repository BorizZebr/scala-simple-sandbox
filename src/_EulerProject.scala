/**
  * Created by borisbondarenko on 26.03.16.
  */
object _EulerProject extends App {

  // first
  def multiply(n: Int): Int = 1 until n filter(i => i % 3 == 0 || i % 5 == 0) sum

  // second
  lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map {n => n._1 + n._2}
  val s = (fibs takeWhile(_< 4000000) filter(x => x % 2 == 0)).sum
  println(s)
}