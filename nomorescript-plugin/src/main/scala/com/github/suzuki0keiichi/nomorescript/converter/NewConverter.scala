package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptNew

trait NewConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toNew(nw: New, scopedVars: ScopedVariables, returnValue: Boolean) = {
    NoMoreScriptNew(toTree(nw.tpt, scopedVars, false))
  }
}