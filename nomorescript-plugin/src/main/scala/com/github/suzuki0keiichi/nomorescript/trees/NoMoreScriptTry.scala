package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTry(block: NoMoreScriptTree, catcheBlock: List[NoMoreScriptTree], finallyBlock: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean): List[String] = {
    List("try {") :::
      block.toJs(true).map("  " + _) :::
      (catcheBlock match {
        case Nil => Nil
        case _ => "} catch (_) {" :: catcheBlock.flatMap(_.toJs(true)).map("  " + _)
      }) :::
      (finallyBlock.toJs(true) match {
        case Nil => Nil
        case js: List[String] => "} finally {" :: js.map("  " + _)
      }) ::: List("}")
  }
}
