package euler

object P34 {

  val facts = Map(
    0 -> 1,
    1 -> 1,
    2 -> 2,
    3 -> 6,
    4 -> 24,
    5 -> 120,
    6 -> 720,
    7 -> 5040,
    8 -> 40320,
    9 -> 362880
  )

  def apply() {
    // we can stop when adding 362880 (9!) is less than
    // multiplying by ten.
    val is = for {
      i <- 3 to 41000 
      ds = digits(i)
      if !ds.exists {d => facts(d) >= i}
      if isCurious(i)
    } yield i
    println(is.sum)
  }  
  
  def isCurious(n: Long) = n == digits(n).map(facts).sum
  
  def digits(n: Long) = n.toString.toSeq.map(_.toString.toInt)
}
