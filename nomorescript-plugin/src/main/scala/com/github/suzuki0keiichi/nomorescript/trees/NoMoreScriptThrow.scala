package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptThrow(id: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    Util.addFirst(id.toJs(true), "throw ")
  }
}