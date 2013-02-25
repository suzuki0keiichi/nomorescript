package sandbox

case class NsList(children: List[NsEmpty], needEmptyLine: Boolean) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    children.flatMap(_.toJs(option)) ::: (if (needEmptyLine) {
      List("")
    } else {
      Nil
    })
  }
}
