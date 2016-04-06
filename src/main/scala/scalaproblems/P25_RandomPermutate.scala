package scalaproblems

import scala.reflect.ClassTag
import scala.util.Random
import scalaproblems.TestRuns._

/**
  * Created by borisbondarenko on 06.04.16.
  */
object P25_RandomPermutate extends App {

  def randomPermutate[T: ClassTag](list: List[T]): List[T] = {

    val rnd = new Random
    val arr = list.toArray
    val l = arr.length - 1

    for(i <- 0 to l) {

      val k = rnd.nextInt(l)
      val elem = arr(i)
      arr.update(i, arr(k))
      arr.update(k, elem)
    }
    arr.toList
  }

  val ls = testIntList
  println(ls)
  time("Shuffle", randomPermutate(ls))
}
