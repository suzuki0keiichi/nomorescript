package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptIf(cond: NoMoreScriptTree, thenp: NoMoreScriptTree, elsep: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val elseJs = elsep.toJs(true).map("  " + _)

    List("if (" + cond.toJs(false).mkString(" ") + ") {") :::
      thenp.toJs(true).map("  " + _) ::: (if (!elseJs.isEmpty && (elseJs.size > 1 || elseJs(0).trim() != "();")) List("} else {") ::: elseJs else Nil) :::
      List("}")
  }
}
