package euler

object P10 {
  import java.util.concurrent._
  import scala.math._
  import scalaz.concurrent._
  import scalaz.Scalaz._
  
  implicit val pool = Executors.newFixedThreadPool(3)
  implicit val strategy = Strategy.Executor

  def findPrimes(range: Traversable[Int]): Promise[List[Int]] = {
    val isPrime = (n: Int) => {
      val lim = ceil(sqrt(n)).asInstanceOf[Int]
      !(2 to lim).exists(n % _ == 0)
    }
    promise(range.filter(_ % 2 != 0).filter(isPrime).toList)
  }

  def apply() = {
    try {
      val promises = (2 to 2000000).grouped(500000).map(findPrimes)
      promises.foldLeft(BigInt(2)) {
        val zero = BigInt(0)
        (sum, primes) => sum + primes.get.foldLeft(zero) {_+_}
      }
    } finally {
      pool.shutdown()
    }
  }
}
