package euler

import scala.math.BigInt
import java.lang.Integer.parseInt
import java.lang.String.valueOf

object P20 {
  def apply() = {
    val fact = (BigInt(1) to BigInt(100)).product
    fact.toString.toList.map(c => parseInt(valueOf(c))).sum
  }
}
