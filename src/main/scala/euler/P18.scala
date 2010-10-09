package euler

import scala.math._
        
object P18 {
  def apply() = {
    val rows = """
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
""".trim.lines.toList
    val tri = new Triangle(rows:_*)
    println(tri.sumAt(0, 0))
  }
}

class Triangle(rowStrings: String*) {

  val sum = calcSums(rowStrings:_*)

  def sumAt(row: Int, col: Int) = sum((row, col))
  
  def calcSums(rows: String*) = {
    var sum: Map[(Int, Int), Int] = Map.empty
    val rows = rowStrings.map(_.trim.split(" ").map(_.toInt))

    (rows.size - 1 to 0 by -1).foreach {
      rowIndex => {
        val row = rows(rowIndex)
        (0 until row.size).foreach {
          colIndex => {
            val v = row(colIndex)
            if (rowIndex == rows.size - 1)
              sum += (rowIndex, colIndex) -> row(colIndex)
            else {
              val l: Int = sum((rowIndex + 1, colIndex))
              val r: Int = sum((rowIndex + 1, colIndex + 1))
              sum += (rowIndex, colIndex) -> (v + max(l, r))
            }
          }
        }
      }
    }
    
    sum
  }
}

