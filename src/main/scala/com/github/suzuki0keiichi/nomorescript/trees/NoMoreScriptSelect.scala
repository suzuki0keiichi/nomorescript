package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptSelect(name: String, child: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val childJs = child.toJs(false)

    if (name == "<init>") {
      if (terminate) Util.addLast(childJs, ";") else childJs
    } else if (childJs == Nil) {
      List(name + (if (terminate) ";" else ""))
    } else {
      Util.addLast(childJs, "." + name + (if (terminate) ";" else ""))
    }
  }
}