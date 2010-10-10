package euler

object P1 {
  def apply() = {
    ((1 to 333).map(3*).filter(k => 0 != (k % 5)) ++ (1 to 199).map(5*)).sum
  }
}
