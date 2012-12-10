package euler

object P36 {
  def isPalindromic(n: Int) = {
    val s = n.toString
    s.reverse == s && {
      val bs = n.toBinaryString
      bs.reverse == bs
    }
  }
  
  def apply() {
    val s = Stream.range(0, 1000000, 1).filter(isPalindromic)
    println(s.sum)
  }
}
 
