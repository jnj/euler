package euler

class Fibs extends Iterator[Int] {
  var a = 0
  var b = 1
  
  override def hasNext = true
  
  override def next = {
    val c = a
    a = b
    b += c
    a
  }
}

object P2 {
  def apply() = {
    val f = new Fibs
    f.takeWhile {_ <= 4000000}.filter {_ % 2 == 0}.reduceLeft {_+_}
  }
}
