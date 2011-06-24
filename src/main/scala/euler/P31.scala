package euler

object P31 {
  def apply = {
    val combinations = for {
      one_pence <- 0 to 200
      two_pence <- 0 to 100
      five_pence <- 0 to 40
      ten_pence <- 0 to 20
      twenty_pence <- 0 to 10
      fifty_pence <- 0 to 4
      one_pound <- 0 to 2
      two_pound <- 0 to 1
      if one_pence + 2 * two_pence + 5 * five_pence + 10 * ten_pence + 20 * twenty_pence + 50 * fifty_pence + 100 * one_pound + 200 * two_pound == 200
    } yield (one_pence, two_pence, five_pence, ten_pence, twenty_pence, fifty_pence, one_pound, two_pound)
    println(combinations.toSet.size)  
  }
}
