package euler

object P6 {
  def sumSq(i: Int, j: Int) = {
    (i to j).map(x => BigInt(x) * BigInt(x)).sum
  }
  
  def sqSum(i: Int, j: Int) = {
    Seq((i to j).map(BigInt(_)).sum).map(x => x * x).head
  }

  def apply() = sqSum(1, 100) - sumSq(1, 100)
}
