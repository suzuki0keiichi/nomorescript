package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptJsFunction(params: List[String], body: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("function (" + params.tail.mkString(", ") + ") {") :::
      List("  var " + params.head + " = this;") :::
      body.toJs(true).map("  " + _) :::
      List("}")
  }
}