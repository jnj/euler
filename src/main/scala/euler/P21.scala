package euler

object P21 {
  def divisors(n: Int) = {
    (1 until n).filter(n % _ == 0)
  }
  
  def d(n: Int) = divisors(n).sum
  
  def apply() = {
    val sums = (1 to 10000).map(i => d(i)).toArray
    val seq = 
      for { 
        i <- 1 to 10000
        j <- 1 to 10000
        if i != j
        if sums(i - 1) == j && sums(j - 1) == i
      } yield (i + j)
    seq.sum / 2
  }
}
