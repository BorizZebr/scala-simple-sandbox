package scalaproblems

import java.lang.Math._

import TestRuns._

/**
  * Created by borisbondarenko on 24.04.16.
  */
object P31_41_Primes extends App {

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def isCoprime(a: Int, b: Int): Boolean =
    gcd(a, b) == 1

  implicit class IntImprovements(i: Int) {

    def isPrime: Boolean =
      primes takeWhile {_ <= sqrt(i)} forall { i % _ != 0}

    def totientCount : Int =
      (1 to i).count(isCoprime(_, i))

    def primeFactors : List[Int] = {
      def loop(acc: List[Int], a: Int, p: Stream[Int]) : List[Int] = a.isPrime match {
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

    def goldbach : (Int, Int) =
      primes takeWhile { _ < i } find { a => (i - a).isPrime } flatMap { x => Some(x, i -x) } get

  }

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _ isPrime })


  def listPrimesinRange(r: Range) : List[Int] =
    primes dropWhile { _ < r.head } takeWhile { _ <= r.last } toList

  def printGoldbachList(r: Range) : List[(Int, Int)] = printGoldbachListLimit(r, 0)

  def printGoldbachListLimit(r: Range, l: Int) : List[(Int, Int)] =
    r filter{ n => n > 2 && n % 2 == 0} map { _ goldbach } filter { _._1 >= l } toList

  println(7 isPrime)
  println(gcd(120, 48))
  println(gcd(36, 63))
  println(315 primeFactors)
  println(315 primeFactorMultiplicity)

  println(10 totientCount)
  println(10 totientCountImproved)

  time("Simple totient", 10090 totientCount)
  time("Improved totient", 10090 totientCountImproved)

  println(listPrimesinRange(7 to 31))

  println(34536 goldbach)
  println(printGoldbachListLimit(1 to 2000, 50))
}
