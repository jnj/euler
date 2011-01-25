package euler

object P27 {
  def apply() = {
    val primes = {
      val s = 2 :: (3 to 999 by 2).filter(Primes.isPrime).toList
      s.map {e => Seq(-e, e)}.flatten
    }

    val s = for {
      a <- -999 to 999
      b <- primes
      n = numPrimes(a, b)
      if n > 0
    } yield (a, b, n)
      
    val t = s.reduceLeft {(a, b) => if (a._3 > b._3) a else b}
    t._1 * t._2
  }
  
  def numPrimes(a: Int, b: Int) = {
    val s = Stream.from(0).map {
      n => (n * n) + (a * n) + b
    }.takeWhile(Primes.isPrime)
    s.size
  }
}
