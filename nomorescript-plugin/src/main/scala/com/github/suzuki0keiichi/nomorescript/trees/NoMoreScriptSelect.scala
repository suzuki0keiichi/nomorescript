package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptSelect(name: String, child: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val childJs = child.toJs(false)

    val replacedName = name.replaceAll("super\\$", "__super__.")

    if (name == "<init>") {
      if (terminate) Util.addLast(childJs, ";") else childJs
    } else if (childJs == Nil) {
      List(replacedName + (if (terminate) ";" else ""))
    } else {
      Util.addLast(childJs, "." + replacedName + (if (terminate) ";" else ""))
    }
  }
}
