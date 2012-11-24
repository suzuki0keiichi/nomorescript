package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptFor(begin: String, end: String, step: String, iterator: String, body: NoMoreScriptTree) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean): List[String] = {
    val stepStatement = step match {
      case "1" => " ++"
      case "-1" => " --"
      case _ => " += " + step
    }

    ("for (var " + iterator + " = " + begin + "; " + iterator + " < " + end + "; " + iterator + stepStatement + ") {") +:
      body.toJs(true).map("  " + _) :+ "}"
  }

}
