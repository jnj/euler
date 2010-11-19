package euler

object P23 {
  def isAbundant(n: Int) = (1 until n).filter(n % _ == 0).sum > n
  
  def apply() = {
    val abundants = (1 to 28123).filter(isAbundant)
    val abundantSums = 
      (for {
        i <- abundants
        j <- abundants
        s = i + j
      } yield(s)).toSet
    (1 to 28123).toSet.diff(abundantSums).sum
  }
}
