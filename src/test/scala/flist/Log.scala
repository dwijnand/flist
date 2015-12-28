package flist

class Log(_name: String = "") {
  val startNano = java.lang.System.nanoTime

  private val name = if (_name.isEmpty) "" else s"${_name} "

  private def prefix: String = {
    val time = nanoToHHmmssSSS(java.lang.System.nanoTime - startNano)
    s"$time$name "
  }

  def println() = Predef.println()
  def println(x: Any) = Predef.println(s"$prefix $x")
}
