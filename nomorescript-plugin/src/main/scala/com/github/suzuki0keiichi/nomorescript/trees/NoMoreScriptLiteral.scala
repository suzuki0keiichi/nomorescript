package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptLiteral(value: String, returnValue: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = List((if (returnValue) "return " else "") + value + (if (terminate) ";" else ""))
}