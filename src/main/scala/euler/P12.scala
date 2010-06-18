package euler

object P12 {
  def tri(n: Int) = (BigInt(n + 1) * n) / 2
  
  def factors(n: BigInt) = {
    var l = BigInt(1) :: Nil
    var i = BigInt(2)
    while (i <= n) {
      if (n % i == 0)
        l = i :: l
      i += 1
    }
    l
  }
  
  def apply() {
    Stream.from(1).map(tri).dropWhile {l => factors(l).size < 500} 
  }
}
