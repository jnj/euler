package euler

object P10 {
  import java.util.concurrent._
  import scala.math._
  import scalaz.concurrent._
  import scalaz.Scalaz._
  
  implicit val pool = Executors.newFixedThreadPool(3)
  implicit val strategy = Strategy.Executor

  def sumPrimes(range: Traversable[Int]): Promise[BigInt] = {
    val isPrime = (n: Int) => {
      val lim = ceil(sqrt(n)).asInstanceOf[Int]
      !(2 to lim).exists(n % _ == 0)
    }
    val isOdd = (i: Int) => i % 2 == 1
    val zero = BigInt(0)
    promise(range.filter(isOdd).filter(isPrime).foldLeft(zero) {_+_})
  }

  def apply() = {
    try {
      val promises = (2 to 2000000).grouped(500000).map(sumPrimes)
      promises.foldLeft(BigInt(2)) {(s, p) => s + p.get}
    } finally {
      pool.shutdown()
    }
  }
}
