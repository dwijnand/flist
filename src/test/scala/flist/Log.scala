package flist

final case class Log(_name: String = "") {
  val startNano = java.lang.System.nanoTime


  private def prefix: String = {
    val diff = java.lang.System.nanoTime - startNano
    val time = nanoToHHmmssSSS(diff)
    val name = if (_name.isEmpty) "" else s" ${_name}"
    s"$time$name"
  }

  def println() = Predef.println()
  def println(x: Any) = Predef.println(s"$prefix $x")
}
