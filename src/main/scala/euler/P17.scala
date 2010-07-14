package euler

import scala.math._

object IntWrapper {
  val words = 
    Map(1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four",
        5 -> "five",
        6 -> "six",
        7 -> "seven",
        8 -> "eight",
        9 -> "nine",
        10 -> "ten",
        11 -> "eleven",
        12 -> "twelve",
        13 -> "thirteen",
        14 -> "fourteen",
        15 -> "fifteen",
        16 -> "sixteen",
        17 -> "seventeen",
        18 -> "eighteen",
        19 -> "nineteen",
        20 -> "twenty",
        30 -> "thirty",
        40 -> "forty",
        50 -> "fifty",
        60 -> "sixty",
        70 -> "seventy",
        80 -> "eighty",
        90 -> "ninety")

  val powers = Map(2 -> "hundred", 3 -> "thousand")

  implicit def intToWrappedInt(n: Int) = new IntWrapper(n)
}

class IntWrapper(val n: Int) {
  import IntWrapper._

  def toEnglish: String = {
    words.get(n) match {
      case Some(word) => word
      case None => {
        val buf = new StringBuilder
        val p = n.toString.length - 1
        val scale = pow(10, p).toInt
        val s = n / scale

        if (scale < 100) {
          buf.append((s * scale).toEnglish)
        } else {
          buf.append(s.toEnglish)
          buf.append(powers(p))
        }
        
        if (n > s * scale) {
          if (scale > 10)
            buf.append("and")
          buf.append((n % scale).toEnglish)
        }

        buf.toString
      }
    }
  }
}

object P17 {
  import IntWrapper._
  
  def apply() = {
    Stream.range(1, 1001).map(_.toEnglish.length).sum
  }
}
