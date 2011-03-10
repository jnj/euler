package euler

import java.util.concurrent._
import scala.collection.immutable.Range
import scala.math._
import scalaz._
import scalaz.concurrent._
import scalaz.Scalaz._
import scala.collection.JavaConversions._

object Primes {
  def isPrime(a: Int) = {
    a >= 2 && (2 to ceil(sqrt(a)).toInt).forall(a % _ != 0)
  }

  def primes(start: Int, stop: Int) = {
    implicit val pool = Executors.newFixedThreadPool(2)
    implicit val strat = Strategy.Executor
    val as = Stream.from(start).takeWhile(stop >=).filter(_ % 2 == 1)
    val x = as.grouped(5000).toStream.parMap(_.filter(isPrime))
    val c: Traversable[Int] = x.get.flatten
    c
  }
}
