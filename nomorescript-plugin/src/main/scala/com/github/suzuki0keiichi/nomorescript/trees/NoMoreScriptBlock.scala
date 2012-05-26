package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptBlock(children: NoMoreScriptTree, needEmptyLine: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("{") ::: children.toJs(true).map("  " + _) ::: (if (needEmptyLine) { List("") } else { Nil }) ::: List("}")
  }
}