package euler

object P16 {
  def apply() = {
    (BigInt(1) << 1000).toString.foldLeft(BigInt(0))(_ + _.toString.toInt)
  }
}
