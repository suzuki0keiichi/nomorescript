package com.github.suzuki0keiichi.nomorescript.converter

import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent

trait IdentConverter extends AnnotationHelper with PackageHelper {
  self: SubComponent =>

  import global._

  def convertIdent(ident: Ident, scopedVars: ScopedVariables, returnValue: Boolean) = {
    if (isGlobalClass(ident)) {
      NoMoreScriptEmpty()
    } else {
      val name = if (ident.symbol.isClass) {
        getPackageName(ident.symbol.owner, null) match {
          case Some(packageName) => packageName + "." + ident.name
          case None => ident.name.toString()
        }
      } else {
        scopedVars.getName(ident.symbol)
      }

      NoMoreScriptIdent(name, returnValue)
    }
  }

  private def isGlobalClass(tree: Ident): Boolean = {
    isGlobalClass(tree.tpe.typeSymbol)
  }
}
