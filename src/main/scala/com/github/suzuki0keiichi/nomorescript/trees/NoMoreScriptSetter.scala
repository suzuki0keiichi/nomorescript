package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptSetter(name: String, target: NoMoreScriptTree, val rhs: NoMoreScriptTree) extends NoMoreScriptTree {
  val rootMatcher = "nomorescriptroot\\.[a-zA-Z0-9_]+\\.([a-zA-Z0-9_.]+)$".r

  override def toJs(terminate: Boolean) = {
    val lhsJs = (target.toJs(false).mkString(" ") + "." + name) match {
      case rootMatcher(name) => name
      case name => name
    }

    val rhsJs = rhs.toJs(false)
    val js =
      (if (rhsJs.size > 1) {
        rhsJs.map("  " + _)
        Util.addFirst(rhsJs, lhsJs + " = ")
      } else {
        Util.addFirst(rhsJs, lhsJs + " = ")
      })

    if (terminate) {
      Util.addLast(js, ";")
    } else {
      js
    }
  }
}