package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptJsFunction(params: Map[String, String], body: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("function (" + params.map(_._1).toList.tail.mkString(", ") + ") {") :::
      List("  var " + params.head + " = this;") :::
      body.toJs(true).map("  " + _) :::
      List("}")
  }
}
