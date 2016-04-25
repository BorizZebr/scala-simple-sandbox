package scalaproblems

/**
  * Created by bbondarenko on 3/27/2016.
  */
object TestRuns {

  def time[R](msg: String, block: => R): R = {

    var i = 0
    val n = 1000.0

    val res = block

    val t0 = System.nanoTime()
    while(i < n){

      block
      i = i + 1
    }
    val t1 = System.nanoTime()

    println("\n%s: Elapsed time is %s ms".format(msg, (t1 - t0) / n / 1000000.0))
    println("Result: %s".format(res))
    res
  }

  val testIntList = List(1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4, 5)

  val testCharList = List('a, 'a, 'b, 'b, 'b, 'b, 'b, 'c, 'd, 'd, 'd, 'e)

  val testSimpleList = List('a, 'b, 'a, 'a, 'c)

  val names = List("AAaaa", "Bbbbb", "Cccc", "Dddddd", "Eeeee", "Fffff", "Gggg", "Hhhhh", "Kkkkk", "LLLLlll")
}
