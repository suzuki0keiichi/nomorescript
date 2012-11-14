package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty

trait TypeApplyConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toTypeApply(aply: TypeApply, scopedVars: ScopedVariables, returnValue: Boolean) = {
    aply.fun match {
      case select: Select if (select.name.toString == "$asInstanceOf") =>
        toTree(select.qualifier, scopedVars, returnValue)

      case _ =>
        NoMoreScriptEmpty()
    }
  }

}