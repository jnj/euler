package euler

object P35 {
  import Primes._

  def apply() {
    val xs = Stream.range(3, 1000000, 2).filter(isPrime).filter(allRotationsPrime)
    println(xs.size + 1)
  }

  def allRotationsPrime(i: Int) = rotations(i.toString.toList.map(_.toString.toInt)).forall {
    l => Primes.isPrime(l.mkString("").toInt)
  }

  def rotations[A](l: List[A]) = {
    (0 until l.size).foldLeft((l, List.empty[List[A]])) {
      case (acc, _) => {
        val x = acc._1.tail ++ List(acc._1.head)
        (x, x :: acc._2)
      }
    }._2
  }
}
