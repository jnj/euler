package euler

object P10 {
  import java.util.concurrent._
  import scala.math._
  import scalaz.concurrent._
  import scalaz.Scalaz._
  
  implicit val pool = Executors.newFixedThreadPool(2)
  implicit val strategy = Strategy.Executor 

  def sumPrimes(range: Traversable[Int]) = {
    val isOdd = (i: Int) => i % 2 == 1
    val zero = BigInt(0)
    val primes = Primes.primes(range.head, range.last)
    primes.foldLeft(zero) {_+_}
  }

  def apply() = {
    try {
      val sums = (2 to 2000000).grouped(500000).map(sumPrimes)
      sums.foldLeft(BigInt(2)) {(s, p) => s + p}
    } finally {
      pool.shutdown()
    }
  }
}
