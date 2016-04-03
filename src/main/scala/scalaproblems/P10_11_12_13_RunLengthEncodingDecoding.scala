package src.scala.scalaproblems

import src.scala.TestRuns
import TestRuns._

import scala.annotation.tailrec

/**
  * Created by borisbondarenko on 27.03.16.
  */
object P10_11_12_13_RunLengthEncodingDecoding extends App{

  def decode[A](ls: List[Either[(Int, A), A]]): List[A] = {
    ls flatMap {
      case Left(e) => List.fill(e._1)(e._2)
      case Right(e) => List(e)
    }
  }

  def encode[T](ls : List[T]): List[Either[(Int, T), T]] = {
    @tailrec
    def _encode[A](ls: List[A], res: List[Either[(Int, A), A]]): List[Either[(Int, A), A]] = ls match {
      case Nil => res
      case h :: _ =>
        val (packed, next) = ls span {h==}
        _encode(next,
          (if(packed.length > 1) Left(packed.length, packed.head)
          else Right(packed.head)) :: res)
    }

    _encode(ls, List()) reverse
  }

  val testList = TestRuns.testCharList

  val coded = encode(testList)
  val decoded = decode(coded)

//  println(testList)
//  println(coded)
//  println(decoded)

  time("Encode", encode(testList))
  time("Decode", decode(coded))
}
