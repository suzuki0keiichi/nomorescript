package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptNamespace(name: String, children: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val names = name.split("\\.")

    val defineNamespaces = (for (i <- 0 until names.length) yield {
      val currentNameSpace = names.slice(0, i + 1).mkString(".")
      "if (typeof " + currentNameSpace + " === \"undefined\") { " + currentNameSpace + " = {}; }"
    }).toList

    defineNamespaces ++ ("" :: children.flatMap(_.toJs(true)))
  }
}

object NoMoreScriptNamespace {
  def isEmpty(name: String) = {
    name == "<empty>"
  }
}
