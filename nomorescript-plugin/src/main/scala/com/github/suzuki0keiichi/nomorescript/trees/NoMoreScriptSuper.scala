package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptSuper(child: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val childJs = child.toJs(false)

    Util.addLast(childJs, ".__super__")
  }
}
