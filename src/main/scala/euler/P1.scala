package euler

object P1 {
  def apply() = {
    ((1 to 333).map {3*}.filter {_%5!=0} ++ (1 to 199).map {5*}).reduceLeft {_+_} 
  }
}
