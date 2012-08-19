package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptConstructor(className: String, params: Map[String, String], body: List[NoMoreScriptTree]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val first = className + ".prototype.__new__ = function"

    // TODO:親traitの__new__を実行してない(親クラスの次且つbodyの前に実行しないといけない)
    List("/**", " * @function") :::
      params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      List(first + "(" + params.map(_._1).mkString(", ") + ") {") :::
      body.flatMap(_.toJs(true)).map("  " + _) :::
      List("};", "")
  }

  def toJsForInterface() = {
    val first = "__trait__.prototype.__new__ = function"

    List(first + "(" + params.map(_._1).mkString(", ") + ") {") :::
      body.flatMap(_.toJs(true)).map("  " + _) :::
      List("};", "")
  }
}

