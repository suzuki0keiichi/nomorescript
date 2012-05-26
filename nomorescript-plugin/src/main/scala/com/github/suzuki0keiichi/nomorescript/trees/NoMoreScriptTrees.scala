package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTrees(children: List[NoMoreScriptTree], needEmptyLine: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    children.flatMap(_.toJs(true)) ::: (if (needEmptyLine) { List("") } else { Nil })
  }
}