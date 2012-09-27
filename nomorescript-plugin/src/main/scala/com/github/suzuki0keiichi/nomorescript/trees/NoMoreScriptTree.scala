package com.github.suzuki0keiichi.nomorescript.trees

trait NoMoreScriptTree {
  def toJs(terminate: Boolean): List[String] = Nil
}

case class NoMoreScriptEmpty() extends NoMoreScriptTree {
}
