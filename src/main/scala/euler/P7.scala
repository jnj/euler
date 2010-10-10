package euler

import Stream._

object P7 {
  def sieve(s: Stream[Int]): Stream[Int] = {
    Stream.cons(s.head, sieve(s.tail filter(_ % s.head > 0)))
  }

  // this lazy sieve approach nukes a 1G heap when going for the 6001st
  def apply() = {
    sieve(Stream.from(2)).drop(6000).head
  }
  
  def apply2() = {
    Stream.from(2).filter(n => P3.factorize(BigInt(n)).size == 1)(10000)
  }
}
