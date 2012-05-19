package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptInstanceOf(target: NoMoreScriptTree, typeName: String) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = Util.addLast(target.toJs(false), " instanceof " + typeName + (if (terminate) { ";" } else { "" }))
}