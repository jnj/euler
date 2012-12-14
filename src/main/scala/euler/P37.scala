package euler

object P37 {
  import Primes._

  def apply() {
    val s = Stream.range(11, 800000, 2).filter(isTruncatablePrime).take(11).sum
    println(s)
  }

  def isTruncatablePrime(n: Int) = {
    n > 7 && isPrime(n) && {
      val digits = n.toString.toList.map(_.toString.toInt)
      truncate(digits).forall(isPrime)
    }
  }  
  
  
  /**
   * Given the digits of a number (as a List), return the numbers that
   * result from left and right truncation.
   *
   * E.g. List(1,3,9) -> List(39, 9, 13, 1)
   */
  def truncate(digits: List[Int]): List[Int] = {
    val left = digits.tails.toList.tail.init
    val right = digits.reverse.tails.toList.tail.init.map(_.reverse)
    (left ++ right).map(_.mkString("").toInt)
  }
}
