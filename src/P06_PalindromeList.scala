/**
  * Created by borisbondarenko on 27.03.16.
  */
object P06_PalindromeList extends App{

  def isPalindrome[T](list: List[T]): Boolean = list == list.reverse

  val list = List(1, 2, 3, 4, 3, 2, 1)
  println(isPalindrome(list))
}
