package scalaproblems

import java.lang.Math._

import TestRuns._

/**
  * Created by borisbondarenko on 24.04.16.
  */
object P31_38_Primes extends App {

  def isPrime(a: Int): Boolean =
    primes takeWhile {_ <= sqrt(a)} forall { a % _ != 0}

  val primes = Stream.cons(2, Stream.from(3, 2) filter { isPrime })

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def isCoprime(a: Int, b: Int): Boolean =
    gcd(a, b) == 1

  implicit class IntImprovements(i: Int) {
    def totientCount : Int =
      (1 to i).count(isCoprime(_, i))

    def primeFactors : List[Int] = {
      def loop(acc: List[Int], a: Int, p: Stream[Int]) : List[Int] = isPrime(a) match {
        case true => a :: acc
        case false =>
          a % p.head == 0 match {
            case true => loop(p.head :: acc, a / p.head, p)
            case false => loop(acc, a, p.tail)
          }
      }
      loop(Nil, i, primes) reverse
    }

    def primeFactorMultiplicity : Map[Int, Int] =
      i.primeFactors.groupBy(x => x).map{ case(k,v) => (k, v.length)}

    def totientCountImproved : Int =
      i.primeFactorMultiplicity.foldLeft(1){ (a, b) =>
        b match { case (k, v) => a * (k - 1) * Math.pow (k, v - 1).toInt }
      }
  }


  println(isPrime(7))
  println(gcd(120, 48))
  println(gcd(36, 63))
  println(315 primeFactors)
  println(315 primeFactorMultiplicity)

  println(10 totientCount)
  println(10 totientCountImproved)

  time("Simple totient", 10090 totientCount)
  time("Improved totient", 10090 totientCountImproved)
}
