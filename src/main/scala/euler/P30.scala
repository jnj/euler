package euler

object P30 {
  val powersOfTen = Array(1, 10, 100, 1000, 10000, 100000, 1000000)
  val powers = Map(
    0 -> 0,
    1 -> 1,
    2 -> 32,
    3 -> 243,
    4 -> 1024,
    5 -> 3125,
    6 -> 7776,
    7 -> 16807,
    8 -> 32768,
    9 -> 59049
  )
  
  def apply() = {
    val l = for {
      i <- 2 to 7
    } yield f(i)

    l.flatten.sum
  }

  def nthDigit(i: Int, n: Int) = {
    (i / powersOfTen(n)) % 10
  }

  def sumPowers(i: Int) = {
    val maxPow = powersOfTen.lastIndexWhere {p => (i / p) > 0}
    (0 to maxPow).map(k => powers(nthDigit(i, k))).sum
  }

  def f(nDigits: Int) = {
    val a = scala.math.pow(10, nDigits - 1).asInstanceOf[Int]
    val b = scala.math.pow(10, nDigits).asInstanceOf[Int]
    for {
      i <- a until b
      if sumPowers(i) == i
    } yield i
  }
}
