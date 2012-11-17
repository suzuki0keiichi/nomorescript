package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptLiteral

trait LiteralConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def convertLiteral(literal: Literal, returnValue: Boolean) = {
    literal.value.value match {
      case s: String => 
        NoMoreScriptLiteral("\"" + literal.value.value.toString + "\"", returnValue)

      case _ =>
        if (literal.value.value == null) {
          NoMoreScriptLiteral("null", returnValue)
        } else {
          NoMoreScriptLiteral(literal.value.value.toString, returnValue)
        }
    }
  }
}
