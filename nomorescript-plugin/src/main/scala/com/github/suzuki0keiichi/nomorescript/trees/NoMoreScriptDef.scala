package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptDef(name: String, params: List[String], body: NoMoreScriptTree, className: Option[String]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = className match {
      case Some(className) => className + ".prototype." + name + " = function"
      case None => "function " + name
    }

    List(first + "(" + params.mkString(", ") + ") {") :::
      body.toJs(true).map("  " + _) :::
      List("}", "")
  }
}