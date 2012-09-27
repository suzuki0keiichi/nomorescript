package com.github.suzuki0keiichi.nomorescript.trees

case class NoMoreScriptCases(cases: List[NoMoreScriptIf]) extends NoMoreScriptTree {
  override def toJs(terminate: Boolean): List[String] = {
    var first = true

    val js = cases.flatMap { c =>
      val head = if (first) {
        first = false
        "if (" + c.cond.toJs(false).mkString(" ") + ") {"
      } else if (c.cond.getClass == classOf[NoMoreScriptEmpty]) {
        "} else {"
      } else {
        "} else if (" + c.cond.toJs(false).mkString(" ") + ") {"
      }

      head :: c.thenp.toJs(true).map("  " + _)
    }
    
    js match {
      case Nil => Nil
      case _ => js :+ "}"
    }
  }
}
