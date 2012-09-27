package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptJsFunction(thisParamName: Option[String], params: Map[String, String], body: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    List("function(" + params.map(_._1).toList.mkString(", ") + ") {") :::
      (thisParamName match {
        case Some(thisParamName) => List("  var " + thisParamName + " = this;")
        case _ => Nil
      }) :::
      body.toJs(true).map("  " + _) :::
      List("}")
  }
}
