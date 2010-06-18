package euler

import java.util.ArrayList
import java.util.concurrent._

import scala.math._

class PrimesFinder(start: Int, stop: Int) extends Callable[List[Int]] {
  def isPrime(n: Int) = !(2 to ceil(sqrt(n)).asInstanceOf[Int]).exists(n % _ == 0)

  def call() = Stream.range(start, stop).filter(isPrime).toList
}

object P10 {
  def apply() = {
    val execSvc = Executors.newFixedThreadPool(2)
    val callables = new ArrayList[Callable[List[Int]]]
    callables.add(new PrimesFinder(2, 500000))
    callables.add(new PrimesFinder(500001, 1000000))
    callables.add(new PrimesFinder(1000001, 1500000))
    callables.add(new PrimesFinder(1500001, 2000000))
    val futures = execSvc.invokeAll(callables)
    val it = futures.iterator()
    var sum = BigInt(0)
    while (it.hasNext()) {
      sum += it.next().get().foldLeft(BigInt(0)) {_+_}
    }
    sum
  }
}
