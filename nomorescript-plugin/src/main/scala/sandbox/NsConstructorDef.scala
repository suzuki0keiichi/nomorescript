package sandbox

case class NsConstructorDef(className: String) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    List("/**", " * @constructor", " */", className + " = function() {", "};", "")
  }
}
