package euler

object P29 {
  def apply() = {
    val as = for {
      a <- 2 to 100
      b <- 2 to 100
      i = BigInt(a).pow(b)
    } yield i
    as.toSet.size
  }
}
