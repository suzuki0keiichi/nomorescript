package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptClass(
  name: String,
  namespace: Option[String],
  constructor: NoMoreScriptConstructor,
  children: List[NoMoreScriptTree]) extends NoMoreScriptTree {

  override def toJs(terminate: Boolean) = {
    (namespace.map(_ + ".").getOrElse("") + name + " = (function() {") ::
      constructor.toJs(false).map("  " + _) :::
      children.flatMap(_.toJs(true)).map("  " + _) :::
      List(
        "  return " + name + ";",
        "})();",
        "")
  }
}