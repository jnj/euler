package euler

import scala.collection.JavaConversions._
import scala.io.Source

object P22 {
  val alpha = Map(
    ('a' -> 1),('b' -> 2),('c' -> 3),('d' -> 4),('e' -> 5),('f' -> 6),
    ('g' -> 7),('h' -> 8),('i' -> 9),('j' -> 10),('k' -> 11),('l' -> 12),
    ('m' -> 13),('n' -> 14),('o' -> 15),('p' -> 16),('q' -> 17),('r' -> 18),
    ('s' -> 19),('t' -> 20),('u' -> 21),('v' -> 22),('w' -> 23),('x' -> 24),
    ('y' -> 25),('z' -> 26)
  )
  
  def alphVal(name: String) = {
    name.toCharArray.map { c => alpha(c.toLower) }.sum
  }

  def stripQuotes(s: String) = s.slice(1, s.size - 1)

  def apply() = {
    val file = "names.txt"
    val resource = Thread.currentThread.getContextClassLoader.getResource(file)
    val lines = Source.fromURL(resource).getLines.toList

    val names = lines.flatMap {
      line => line.split(',').map(stripQuotes(_))
    }

    val sorted = names.sortWith(_<_)
    sorted.zipWithIndex.map {
      case (name, idx) => (idx + 1) * alphVal(name)
    }.sum
  }
}


















