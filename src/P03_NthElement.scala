import TestRuns.time

/**
  * Created by borisbondarenko on 26.03.16.
  */
object P03_NthElement extends App {

  def nthElement[T](ls : List[T], n: Int): T = (n, ls) match {
    case(0, h :: _) => h
    case(i, h :: t) => nthElement(t, i - 1)
    case(_, Nil) => throw new NoSuchElementException
  }


  time("Nth rec", nthElement(TestRuns.testIntList, 5))
  time("Nth stabdard", TestRuns.testIntList(5))
}
