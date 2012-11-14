package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.ast.Trees
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptThis
import scala.tools.nsc.symtab.Types
import scala.reflect.generic.Symbols
import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent

trait ThisConverter extends ConverterBase with AnnotationHelper {
  self: SubComponent =>

  import global._

  def isGlobalClass(tree: This): Boolean = {
    isGlobalClass(tree.tpe.typeSymbol)
  }

  def toThis(ths: This, returnValue: Boolean) = {
    if (isGlobalClass(ths)) {
      NoMoreScriptEmpty()
    } else {
      NoMoreScriptThis(returnValue)
    }
  }
}