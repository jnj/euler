package euler

object P32 {
  val digits = "123456789".toSet

  def isPandigital(n: Int) = n.toString.toSet == digits
  
  def apply() {
    val sums = for {
      i <- (2 to 98)
      j <- (123 to 9876)
      k = i * j
      s = i.toString + j.toString + k.toString
      if s.size == 9
      if isPandigital(s.toInt)
    } yield (i, j)

    sums.foreach {
      case (i, j) => println("%s * %s = %s".format(i, j, i * j))
    }

    println(sums.map {case (i, j) => i*j}.toSet)
    println(sums.map {case(i, j) => i*j}.toSet.sum)
  }
}
