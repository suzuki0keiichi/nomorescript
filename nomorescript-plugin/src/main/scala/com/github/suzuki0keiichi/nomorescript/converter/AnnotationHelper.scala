package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent

trait AnnotationHelper {
  self: SubComponent =>

  import global._

  def haveAnnotation(cdef: ClassDef, name: String): Boolean = haveAnnotation(cdef.symbol, name)

  def haveAnnotation(symbol: Symbol, name: String): Boolean = symbol.annotations.exists(_.toString == name)

  def isGlobalClass(typeSymbol: Symbol): Boolean = haveAnnotation(typeSymbol, "com.github.suzuki0keiichi.nomorescript.annotation.global")
}