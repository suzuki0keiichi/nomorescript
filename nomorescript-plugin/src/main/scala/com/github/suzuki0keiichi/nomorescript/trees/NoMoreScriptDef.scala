package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptDef(name: String, params: Map[String, String], body: NoMoreScriptTree, className: Option[String]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = className match {
      case Some(className) => className + ".prototype." + name + " = function"
      case None => "function " + name
    }

    List("/*", " * @function") :::
      params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      List(first + "(" + params.map(_._1).toList.mkString(", ") + ") {") :::
      body.toJs(true).map("  " + _) :::
      List("}", "")
  }
}
