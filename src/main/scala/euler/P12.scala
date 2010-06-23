package euler

object P12 {
  def tri(n: BigInt) = ((n + 1) * n) / 2
  
  def factors(n: BigInt) = {
    var l = n :: 1 :: Nil
    var i = if (n == 2) BigInt(3) else BigInt(2)

    while (i * i <= n) {
      val pair = n /% i
      if (pair._2 == 0)
        l = pair._1 :: i :: l
      i += 1
    }

    l
  }
  
  def apply() = {
    var i = BigInt(1)
    var t = BigInt(0)
    var done = false

    while (!done) {
      t = tri(i)
      
      if (factors(t).size >= 500)
        done = true
      else
        i = i + 1
    }
    
    t
  }
}
