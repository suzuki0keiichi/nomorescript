package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTraitDef(name: String, paramss: List[Map[String, String]], body: NoMoreScriptTree, traitName: String) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = "__trait__.prototype." + name + " = function"

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
      List(if (terminate) "};" else "}", "")
  }

  def toJsForInterface(): List[String] = {
    List("/*", " * @function") :::
      paramss.head.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      List(traitName + ".prototype." + name + " = function(" + paramss.head.map(_._1).toList.mkString(", ") + "){};", "")
  }
}