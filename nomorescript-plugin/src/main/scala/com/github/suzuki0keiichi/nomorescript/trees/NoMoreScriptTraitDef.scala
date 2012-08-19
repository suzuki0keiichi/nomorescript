package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTraitDef(name: String, params: Map[String, String], body: NoMoreScriptTree, traitName: String) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = "__trait__.prototype." + name + " = function"

    List(first + "(" + params.map(_._1).toList.mkString(", ") + ") {") :::
      body.toJs(true).map("  " + _) :::
      List(if (terminate) "};" else "}", "")
  }

  def toJsForInterface(): List[String] = {
    List("/*", " * @function") :::
      params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      List(traitName + ".prototype." + name + " = function(" + params.map(_._1).toList.mkString(", ") + "){};", "")
  }
}