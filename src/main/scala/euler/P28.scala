package euler

class Diagonals extends Iterator[Int] {
  private var i = 1
  private var e = 2
  private var c = 0
  
  override def hasNext = true
  
  override def next() = {
    val k = i
    e = 2 + (c / 4) * 2
    c += 1
    i += e
    k
  }
}

object P28 {
  def apply() = {
    val lim = 1001 * 1001
    val diag = new Diagonals
    diag.takeWhile(lim >=).sum
  }
}
