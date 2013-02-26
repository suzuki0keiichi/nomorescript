package sandbox

case class NsVariable(name: String, rhs: NsEmpty, isMember: Boolean) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    List((if (isMember) "this." else "var ") + name + " = " + rhs.toJs(option).mkString(" ") + ";")
  }
}
