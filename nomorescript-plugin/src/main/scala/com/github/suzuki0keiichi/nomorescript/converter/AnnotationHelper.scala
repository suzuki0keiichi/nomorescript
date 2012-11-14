package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent

trait AnnotationHelper {
  self: SubComponent =>

  import global._

  def haveAnnotation(cdef: ClassDef, name: String): Boolean = {
    cdef.symbol.annotations.exists(_.toString == name)
  }

  def isGlobalClass(typeSymbol: Symbol): Boolean = {
    typeSymbol.annotations.exists(_.toString == "com.github.suzuki0keiichi.nomorescript.annotation.global")
  }
}