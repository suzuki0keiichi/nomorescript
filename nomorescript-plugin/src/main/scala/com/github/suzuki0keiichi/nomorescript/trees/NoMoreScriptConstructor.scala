package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptConstructor(className: String, params: Map[String, String], members: Map[String, String], body: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = className + " = function"

    List(first + "(" + params.map(_._1).mkString(", ") + ") {") :::
      members.map("  this." + _._1 + " = null;").toList :::
      body.flatMap(_.toJs(true)).map("  " + _) :::
      List("};", "")
  }

  def toJsForInterface() = {
    List("/**", " * @interface", " */", className + " = function(" + params.map(_._1).mkString(", ") + ") {") :::
      body.flatMap(_.toJs(true)).map("  " + _) :::
      List("};", "", className + ".__impl__ = {};", "")
  }
}

