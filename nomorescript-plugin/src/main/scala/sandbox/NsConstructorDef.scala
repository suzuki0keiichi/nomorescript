package sandbox

case class NsConstructorDef(className: String, fields: Map[String, String], params: Map[String, String], bodies: List[NsEmpty]) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    List("/**", " * @constructor") :::
      fields.map(field => " * @property {" + field._2 + "} " + field._1).toList :::
      params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */", className + " = function (" + params.map(param => param._1).mkString(", ") + ") {") :::
      bodies.flatMap(_.toJs(option)).map(option.indent + _) :::
      List("};", "")
  }
}
