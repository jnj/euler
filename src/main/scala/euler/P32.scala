package euler

object P32 {
  val digits = "123456789".toSet

  def isPandigital(n: Int) = n.toString.toSet == digits
  
  def apply() {
    val sums = for {
      i <- (11 to 99)
      j <- (101 to 999)
      k = i * j
      if k < 9999
      s = i.toString + j.toString + k.toString
      if s.size == 9
      if isPandigital(s.toInt)
    } yield k
    
    println(sums.toSet.sum)
  }
}
