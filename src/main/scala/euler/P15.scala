package euler

object P15 {
  def fact(n: BigInt) = {
    var k = n
    var p = BigInt(1)
    while (k > 1) {
      p *= k
      k -= 1
    }
    p
  }

  def countPaths(m: BigInt, n: BigInt) = {
    fact(m + n) / (fact(m) * fact(n))
  }

  def apply() = {
    countPaths(20, 20)
  }
}
