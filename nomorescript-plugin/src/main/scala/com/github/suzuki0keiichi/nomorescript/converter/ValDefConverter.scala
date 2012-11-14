package com.github.suzuki0keiichi.nomorescript.converter

import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptVal
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent
import scala.tools.nsc.SubComponent

trait ValDefConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toPrimitiveType(typeName: String) = {
    if (typeName.indexOf("=>") != -1) {
      "Function"
    } else {
      PrimitiveTypes.get(typeName).getOrElse(typeName)
    }
  }

  def toVal(vdef: ValDef, scopedVars: ScopedVariables, memberNames: Map[String, String]) = {
    val isMember = memberNames.contains(vdef.name.toString.trim())
    val name = if (isMember) {
      vdef.name.toString.trim()
    } else {
      scopedVars.put(vdef.name.toString.trim(), vdef.symbol)
    }

    if (vdef.rhs.toString.trim() == "<empty>") {
      if (isMember) {
        NoMoreScriptVal(name, NoMoreScriptIdent(name, false), isMember)
      } else {
        NoMoreScriptVal(name, NoMoreScriptIdent("null", false), false);
      }
    } else {
      NoMoreScriptVal(name, toTree(vdef.rhs, scopedVars, false), isMember)
    }
  }
}