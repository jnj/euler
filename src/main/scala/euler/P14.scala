package euler

import java.util.ArrayList
import java.util.concurrent._

import scala.collection.JavaConversions._

class SequenceFinder(val rangeStart: BigInt, val rangeEnd: BigInt) 
  extends Callable[(BigInt, BigInt)] {
    
  def call() = {
    var table = Map(BigInt(1) -> BigInt(1))
    var i = rangeStart
    
    while (i <= rangeEnd) {
      table += (i -> sequenceLength(i, table))
      i += 1
    }
    
    table.max(Ordering[BigInt].on[(_, BigInt)](_._2))
  }  
    
  def f(n: BigInt) = {
    if (n % 2 == 0)
      n / 2
    else
      n * 3 + 1
  }
  
  // 3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
  def sequenceLength(n: BigInt, table: Map[BigInt, BigInt]): BigInt = {
    var i = n
    var len = BigInt(0)

    while (i >= 1) {
      table.get(i) match {
        case None => {
          len += 1
          i = f(i)
        }
        case Some(k) => return len + k
      }
    }

    len
  }
}

object P14 {
  
  def apply() = {
    val execSvc = Executors.newFixedThreadPool(2)
    val callables = new ArrayList[Callable[(BigInt, BigInt)]]

    callables.add(new SequenceFinder(1, 250000))
    callables.add(new SequenceFinder(250001, 500000))
    callables.add(new SequenceFinder(500001, 750000))
    callables.add(new SequenceFinder(750001, 999999))
    
    val futures = execSvc.invokeAll(callables)
    val results = futures.iterator.map(_.get)

    results.foreach(println)
  }
}
