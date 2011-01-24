package euler

object P27 {
  val pf = new PrimesFinder(0, 0)

  def apply() = {
    val primes = {
      val l = 2 :: (3 to 999 by 2).filter(pf.isPrime).toList
      l.flatMap {k => Seq(k, -k)}
    }

    val s = 
      for {
        a <- (-999 to 999)
        b <- primes
        n = numPrimes(a, b)
        if n > 0
      } yield (a, b, n)
    s.reduceLeft {(a, b) => if (a._3 > b._3) a else b}
  }
  
  def numPrimes(a: Int, b: Int) = {
    val s = Stream.from(0).map {
      // n^2 + an + b
      n => (n * n) + (a * n) + b
    }.takeWhile(pf.isPrime)
    //println(s.mkString(", "))
    s.size
  }
}
