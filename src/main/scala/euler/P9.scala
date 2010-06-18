package euler

import scala.math._

object P9 {
  def apply() = {
    val tuples = 
      for {a <- 1 to 999
         b <- 1 to 1000 - a
         c <- 1 to round(sqrt(a * a + b * b)).asInstanceOf[Int]
         if (a * a + b * b == c * c);
         if (a + b + c == 1000)} yield List(a,b,c)
    tuples.map {t => t.reduceLeft {_*_}}
  }
}
