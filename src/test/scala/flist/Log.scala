package flist

class Log(_name: String = "") {
  private def isoTime() = {
    val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    df setTimeZone (java.util.TimeZone getTimeZone "UTC")
    df format new java.util.Date()
  }

  private val name = if (_name.isEmpty) "" else s"${_name} "

  private def prefix: String = s"$isoTime$name "

  def println() = Predef.println()
  def println(x: Any) = Predef.println(s"$prefix $x")
}
