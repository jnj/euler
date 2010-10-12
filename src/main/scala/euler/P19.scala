package euler

object P19 {

  val days = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def apply() = {
    var month = 0
    var dow = 0 // 0 is Monday
    var dom = 1
    var yr = 1900
    var sundays = 0

    while (yr <= 2000) {
      val daysInMonth = 
        if (month == 1 && isLeapYear(yr))
          29
        else
          days(month)

      if (dom == daysInMonth) {
        dom = 1
        dow = (dow + 1) % 7
        yr = if (month == 11) yr + 1 else yr
        month = (month + 1) % 12
        if (dow == 6 && yr >= 1901)
          sundays = sundays + 1
      } else {
        dom = dom + 1
        dow = (dow + 1) % 7
      }
    }
    
    println(sundays)
  }
  
  def isLeapYear(year: Int) = {
    (year % 100 == 0 && year % 400 == 0) || year % 4 == 0
  }
}
