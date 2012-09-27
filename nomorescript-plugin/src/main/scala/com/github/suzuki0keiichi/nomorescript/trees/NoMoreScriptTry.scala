package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptTry(block: NoMoreScriptTree, catcheBlock: NoMoreScriptCases, finallyBlock: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean): List[String] = {
    "try {" ::
      block.toJs(true).map("  " + _) ++ 
      Util.mapOrNil(catcheBlock.toJs(true).map("  " + _), "} catch (__match_target__) {" :: _) ++
      Util.mapOrNil(finallyBlock.toJs(true), "} finally {" :: _.map("  " + _)) :+ "}"
  }
}
