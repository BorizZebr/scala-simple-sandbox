/**
  * Created by bbondarenko on 3/28/2016.
  */
object RNATest extends App {

  val rna = RNA1.fromSeq(Seq(C, A, A, U, G, C))

  val tri = rna(4)
  println(tri)
}