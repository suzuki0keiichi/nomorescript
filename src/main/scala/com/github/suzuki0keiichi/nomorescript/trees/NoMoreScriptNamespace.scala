package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptNamespace(name: String, children: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("namespace([" + name.split("\\.").map("\"" + _ + "\"").mkString(", ") + "]);", "") ::: children.flatMap(_.toJs(true))
  }
}

object NoMoreScriptNamespace {
  def isEmpty(name: String) = {
    name == "<empty>"
  }
}