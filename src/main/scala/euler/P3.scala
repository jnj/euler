package euler

object P3 {
  def firstFactor(n: BigInt): Option[BigInt] = {
    var m = BigInt(2)
    val p = n
    while (m < p) {
      val (q, r) = n /% m
      if (r == 0)
        return Some(q)
      else
        m = m + 1
    }
    None
  }

  def factorize(n: BigInt): Seq[BigInt] = {
    firstFactor(n) match {
      case None => Seq(n)
      case Some(k) => factorize(k) ++ factorize(n / k)
    }
  }

  def apply() = {
    var i = BigInt("600851475143")
    println(factorize(i).reduceLeft { (b,a) => if (a > b) a else b })
  }
}
