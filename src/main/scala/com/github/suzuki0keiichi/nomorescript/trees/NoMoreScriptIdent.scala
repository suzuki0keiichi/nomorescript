package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptIdent(id: String, returnValue: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = List((if (returnValue) "return " else "") + id + (if (terminate) ";" else ""))
}