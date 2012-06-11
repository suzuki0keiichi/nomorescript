package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptClass(
  name: String,
  namespace: Option[String],
  constructor: NoMoreScriptConstructor,
  parents: List[String],
  children: List[NoMoreScriptTree]) extends NoMoreScriptTree {

  override def toJs(terminate: Boolean) = {
    val fullName = namespace.map(_ + ".").getOrElse("") + name

    (fullName + " = (function() {") ::
      constructor.toJs(false).map("  " + _) :::
      children.flatMap(_.toJs(true)).map("  " + _) :::
      List(
        "  return " + name + ";",
        "})();",
        "") :::
        (parents.reverse.map("mixin(" + fullName + ", " + _ + ");") match {
          case Nil => Nil
          case list: List[String] => list ::: List("")
        })
  }
}