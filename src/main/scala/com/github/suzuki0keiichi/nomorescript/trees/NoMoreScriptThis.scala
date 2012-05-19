package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptThis(returnValue: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = List((if (returnValue) "return " else "") + "this" + (if (terminate) ";" else ""))
}