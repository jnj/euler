package euler

object P4 {
  def isPalindromic(n: Int): Boolean = {
    val s = n.toString
    s == s.reverse.toString
  }

  def apply() {
    val seq = for {i <- Stream.range(999, 99, -1)
                   j <- Stream.range(999, 99, -1)} 
                yield (i * j)
    val x = seq filter { isPalindromic } reduceLeft {(a,b) => if (a > b) a else b}
    println(x)
  }
}
