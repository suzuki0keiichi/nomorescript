package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptApply(fun: NoMoreScriptTree, params: List[NoMoreScriptTree], returnValue: Boolean, isArrayApply: Boolean) extends NoMoreScriptTree {
  val rootMatcher = "(new |)nomorescriptroot\\.[a-zA-Z0-9_]+\\.([a-zA-Z0-9_.]+)$".r
  val arrayApplyMatcher = "scala\\.(Array|Map)\\.apply".r
  val applyMatcher = "([a-zA-Z0-9_.]+)\\.apply".r

  override def toJs(terminate: Boolean) = {
    val childJs = (fun.toJs(false) match {
      case js if (js.size > 1) =>
        js.last match {
          case arrayApplyMatcher(_) => js.init ::: List("")
          case applyMatcher(name) => js.init ::: List(name)
          case _ => js
        }

      case js =>
        js(0) match {
          case arrayApplyMatcher(_) => List("")
          case applyMatcher(name) => List(name)
          case _ => js
        }
    }) match {
      case js if (js.size > 1) =>
        js.first match {
          case rootMatcher(ope, name) => List(ope + name) ::: js.tail
          case _ => js
        }

      case js =>
        js(0) match {
          case rootMatcher(ope, name) => List(ope + name)
          case _ => js
        }
    }

    val paramCsString = if (params.isEmpty) {
      ""
    } else {
      val thisJs = params.flatMap(_.toJs(false))
      if (!thisJs.isEmpty) {
        thisJs.reduceLeft(_ + ", " + _)
      } else {
        ""
      }
    }

    val childJs2 = if (returnValue) Util.addFirst(childJs, "return ") else childJs

    if (isArrayApply) {
      Util.addLast(childJs2, "[" + paramCsString + "]" + (if (terminate) ";" else ""))
    } else {
      Util.addLast(childJs2, "(" + paramCsString + ")" + (if (terminate) ";" else ""))
    }
  }
}