package euler

import scala.collection.mutable.ArrayBuffer

object P24 {

  def apply() = {
    val a = ArrayBuffer(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    var i = 1
    while (i < 1000000) {
      next(a)
      i +=1
    }
    println(a)
  }

  def next(xs: ArrayBuffer[Int]) {
    val n = xs.size
    var l = n - 2
    while (l >= 0 && xs(l) > xs(l+1)) {
      l -= 1
    }
    if (l >= 0) {
      var r = n - 1
      while (xs(r) < xs(l)) {
        r -= 1
      }
      swap(l, r, xs)
      r = n - 1
      l += 1
      while (l < r) {
        swap(l, r, xs)
        l += 1
        r -= 1
      }
    }
  }
  
  def swap(i: Int, j: Int, xs: ArrayBuffer[Int]) {
    val c = xs(i)
    xs(i) = xs(j)
    xs(j) = c
  }
}
