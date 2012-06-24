package euler

import scala.math.BigInt

case class Fraction(n: Int, d: Int) {
  def reduced = {
    val gcd = BigInt(n).gcd(BigInt(d)).intValue
    Fraction(n / gcd, d / gcd)
  }

  def *(that: Fraction) = {
    Fraction(n * that.n, d * that.d)
  }

  def simplified: Option[Fraction] = {
    val ns = n.toString
    val ds = d.toString
    val common = (ns.toSet & ds.toSet)

    for {
      x <- common.toList.headOption
      if x != '0'
      if ns.toList.count { _ == x } == 1
      if ds.toList.count { _ == x } == 1
      nn = ns.replaceAll(x.toString, "").toInt
      nd = ds.replaceAll(x.toString, "").toInt
    } yield Fraction(nn, nd)
  }
  
  override def toString = "(%s/%s)".format(n, d)
}

object P33 {
  def apply() {
    val fracs = for {
      i <- (10 to 99)
      j <- (10 to 99)
      if i < j
      if (i.toString.toSet & j.toString.toSet).size > 0
      f = Fraction(i, j)
      g <- f.simplified
      if f.reduced == g.reduced
    } yield f
  
    println(fracs.foldLeft(Fraction(1,1)) {_*_}.reduced.d)
  }
}
