package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptDef(name: String, paramss: List[Map[String, String]], body: NoMoreScriptTree, className: Option[String]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = className match {
      case Some(className) => className + ".prototype." + name + " = function"
      case None => "function " + name
    }

    List("/*", " * @function") :::
      paramss.head.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      List(first + "(" + paramss.head.map(_._1).toList.mkString(", ") + ") {") :::
      (if (paramss.length > 1) {
        paramss.tail.flatMap {
          params =>
            List("  /*", "   * function") :::
              params.map(param => "   * @param {" + param._2 + "} " + param._1).toList :::
              List("   */", "  return function(" + params.map(_._1).toList.mkString(", ") + ") {")
        }
      } else {
        Nil
      }) :::
      body.toJs(true).map("  " + _) :::
      (if (paramss.length > 1) {
        paramss.tail.flatMap(params => List("  }"))
      } else {
        Nil
      }) :::
      (className match {
        case Some(className) => List("};", "")
        case _ => List("}", "")
      })
  }
}
