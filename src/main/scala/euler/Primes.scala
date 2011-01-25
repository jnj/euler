package euler

import java.util.concurrent._
import scala.collection.immutable.Range
import scala.math._
import scalaz.concurrent._
import scalaz.Scalaz._

object Primes {
  def isPrime(a: Int) = {
    a >= 2 && (2 to ceil(sqrt(a)).toInt).forall(a % _ != 0)
  }

  def primesOf(as: Traversable[Int]) = as.filter(isPrime)
  
  def primes(as: Range) = {
    implicit val pool = Executors.newFixedThreadPool(5)
    implicit val strat = Strategy.Executor
    as.grouped(1000).map {i => promise(primesOf(i))}
  }
}
