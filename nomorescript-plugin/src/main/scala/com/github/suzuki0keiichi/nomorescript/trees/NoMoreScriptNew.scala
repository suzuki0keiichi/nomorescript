package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptNew(target: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("(new " + target.toJs(false).mkString(" ") + "()).__new__")
  }
}
