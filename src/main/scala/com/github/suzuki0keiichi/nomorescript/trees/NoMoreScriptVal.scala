package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptVal(name: String, rhs: NoMoreScriptTree, isMember: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List((if (isMember) { "this." } else { "var " }) + name + " = " + rhs.toJs(false).mkString(" ") + (if (terminate) ";" else ""))
  }
}