package scalaproblems

import java.lang.Math.sqrt

/**
  * Created by borisbondarenko on 09.04.16.
  */
object P31_Prime extends App {

  def isPrime(a: Int): Boolean =
    primes takeWhile {_ <= sqrt(a)} forall { a % _ != 0}

  val primes = Stream.cons(2, Stream.from(3, 2) filter { isPrime })

  println(isPrime(7))
}
