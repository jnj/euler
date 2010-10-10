package euler

import P3.factorize
import scala.math._

object P5 {
  implicit def bigIntToInt(n: BigInt): Int = n.intValue

  def apply() = {
    val n = 20
    var m = Map.empty[Int, Traversable[Int]]
    val bindings = 
      for {
        i <- 2 to n
      } yield (i -> factorize(i).map(_.intValue))

    m ++= bindings
    val primes = m.keys.filter(m(_).size == 1).toList
    val maxOccurs = (p: Int) => m.keys.map(m(_).filter(p ==).size).max
    val powers = primes.map(maxOccurs)
    primes.zip(powers.toList).map {t => round(pow(t._1, t._2))}.product
  }
}
