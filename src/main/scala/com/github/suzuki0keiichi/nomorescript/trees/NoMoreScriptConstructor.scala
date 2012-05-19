package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptConstructor(className: String, params: List[String], body: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    ("function " + className + "(" + params.mkString(", ") + ") {") ::
      body.flatMap(_.toJs(true)).map("  " + _) :::
      List(
        "}",
        "")
  }
}
