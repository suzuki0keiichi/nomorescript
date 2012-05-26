package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptOperator(operator: String, target1: NoMoreScriptTree, val target2: NoMoreScriptTree, returnValue: Boolean) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean) = {
    val js = (target1.toJs(false) ::: List(operator) ::: target2.toJs(false)).reduceLeft(_ + " " + _)

    List((if (returnValue) "return " else "") + js + (if (terminate) ";" else ""))
  }
}