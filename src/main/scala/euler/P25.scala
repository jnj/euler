package euler

import scala.math.BigInt

class Fibs extends Iterator[BigInt] {
  var a = BigInt(0)
  var b = BigInt(1)
  
  override def hasNext = true
  
  override def next = {
    val c = a
    a = b
    b += c
    a
  }
}

object P25 {
  def apply() {
    val fibs = new Fibs
    fibs.indexWhere(_.toString.size >= 1000) match {
      case -1 => println("none found")
      case i => println(i + 1)
    }
  }
}










