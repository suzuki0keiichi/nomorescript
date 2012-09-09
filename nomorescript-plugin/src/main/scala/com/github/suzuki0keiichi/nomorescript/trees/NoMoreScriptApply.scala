package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptApply(fun: NoMoreScriptTree, params: List[NoMoreScriptTree], returnValue: Boolean, isArrayApply: Boolean) extends NoMoreScriptTree {
  val rootMatcher = "(new |)nomorescriptroot\\.[a-zA-Z0-9_]+\\.([$a-zA-Z0-9_.]+)$".r
  val arrayApplyMatcher = "scala\\.(Array|Map)\\.apply".r
  val applyMatcher = "([a-zA-Z0-9_.]+)\\.apply".r

  override def toJs(terminate: Boolean): List[String] = {
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
        js.head match {
          case rootMatcher(ope, name) => List(ope + name) ::: js.tail
          case _ => js
        }

      case js =>
        js(0) match {
          case rootMatcher(ope, name) => List(ope + name)
          case _ => js
        }
    }

    val head = if (isArrayApply) "[" else "("
    val tail = if (isArrayApply) "]" else ")"
    val childJs2 = if (returnValue) Util.addFirst(childJs, "return ") else childJs
    
    val paramCsString = if (params.isEmpty) {
      ""
    } else {
      val thisJs: List[List[String]] = params.map(_.toJs(false))
      if (!thisJs.flatten.isEmpty) {
        if (thisJs.exists(_.exists(_.endsWith(";")))) {
          // パラメーターの場所に複数行ある場合は特殊な表示方法にする
          return Util.addLast(childJs2, head) ++ thisJs.reduceLeft(_ ::: ", " +: _) :+ (tail + (if (terminate) ";" else ""))
        } else {
          thisJs.flatten.reduceLeft(_ + ", " + _)
        }
      } else {
        ""
      }
    }

    Util.addLast(childJs2, head + paramCsString + tail + (if (terminate) ";" else ""))
  }

  def toSuperConstructorApply() = {
    NoMoreScriptSuperConstructorApply(fun, params)
  }
}

case class NoMoreScriptSuperConstructorApply(fun: NoMoreScriptTree, params: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val paramCsString = if (params.isEmpty) {
      "this"
    } else {
      val thisJs = "this" :: params.flatMap(_.toJs(false))

      thisJs.reduceLeft(_ + ", " + _)
    }

    Util.addLast(fun.toJs(false), ".call(" + paramCsString + ")" + (if (terminate) ";" else ""))
  }
}
