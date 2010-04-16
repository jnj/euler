package euler

import P3.factorize
import Math._

object P5 {
  implicit def intToBigInt(n: Int): BigInt = BigInt(n)
  implicit def bigIntToInt(n: BigInt): Int = n.intValue

  def apply() = {
    val n = 20
    var m = Map.empty[Int, Seq[Int]]

    for (i <- 2 to n)
      m += ((i, factorize(i).map { _.intValue }))

    val max = (a: Int, b: Int) => if (a > b) a else b
    val primes = m.keys.filter {(m(_).size == 1)}.toList
    val maxOccurs = (p: Int) => m.keys.map {m(_).filter {p ==}.size}.reduceLeft(max)
    val powers = primes.map(maxOccurs)
    primes.zip(powers.toList).map {t => round(powt._1, t._2))}.reduceLeft {_*_}
  }
}
