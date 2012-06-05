package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTry(block: NoMoreScriptTree, catcheBlock: NoMoreScriptCases, finallyBlock: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean): List[String] = {
    List("try {") :::
      block.toJs(true).map("  " + _) :::
      (catcheBlock.toJs(true).map("  " + _) match {
        case Nil => Nil
        case js: List[String] => "} catch (_) {" :: js
      }) :::
      (finallyBlock.toJs(true) match {
        case Nil => Nil
        case js: List[String] => "} finally {" :: js.map("  " + _)
      }) ::: List("}")
  }
}
