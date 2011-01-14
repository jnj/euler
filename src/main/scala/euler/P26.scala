package euler

import java.util.concurrent._
import scala.collection.JavaConversions._

object P26 {

  def maxTuple(t: (Int, Int), u: (Int, Int)) = {
    if (t._2 > u._2)
      t
    else
      u
  }

  class Finder(v: Traversable[Int]) extends Callable[(Int, Int)] {
    override def call = v.map(cycLen(_)).reduceLeft(maxTuple)

    def cycLen(den: Int) = {
      var num = 10
      var done = false
      var nums = num :: Nil
      var len = 0

      while (!done) {
        var (quot, rem) = divMod(num, den)

        if (quot == 0) {
          num *= 10
          0
        } else {
          nums = num :: nums
          num = rem * 10

          if (nums.contains(num))
            len = nums.size - nums.reverse.indexWhere(num==)
          
          done = rem == 0 || len > 0
        }
      }
      
      (den, len)
    }

    private def divMod(x: Int, y: Int) = (x / y, x % y)
  }

  def apply() = {
    val execSvc = Executors.newFixedThreadPool(2)
    var callables = List.empty[Callable[(Int, Int)]]

    (3 until 1000).filterNot(_ % 2 == 0).filterNot(_ % 5 == 0).grouped(10).foreach {
      v => callables = new Finder(v) :: callables
    }

    try {
      val futures = execSvc.invokeAll(callables)
      futures.map(_.get).reduceLeft(maxTuple)
    } finally {
      execSvc.shutdown()
    }
  }
} 
