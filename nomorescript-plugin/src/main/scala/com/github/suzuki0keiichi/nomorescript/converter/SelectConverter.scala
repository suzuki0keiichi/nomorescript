package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSelect
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSelect

trait SelectConverter extends ConverterBase with PackageHelper {
  self: SubComponent =>

  import global._

  def toSelect(select: Select, scopedVars: ScopedVariables, returnValue: Boolean) = {
    if (select.name.toString == "package") {
      toTree(select.qualifier, scopedVars, returnValue)
    } else {
      select.symbol match {
        case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
          // TODO:toJsの方でやる
          if (m.referenced.enclClass.isTrait) {
            val name = getPackageName(m.referenced.enclClass, null).get + ".__impl__." + select.name.toString.substring("super$".length) + ".call"
            NoMoreScriptIdent(name, returnValue)
          } else {
            val name = getPackageName(m.referenced.enclClass, null).get + ".prototype." + select.name.toString.substring("super$".length) + ".call"
            NoMoreScriptIdent(name, returnValue)
          }

        case _ =>
          NoMoreScriptSelect(scopedVars.getName(select.symbol), toTree(select.qualifier, scopedVars, returnValue))
      }

    }
  }
}