package com.github.suzuki0keiichi.nomorescript.converter
import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent

trait ConstructorHelper {
  self: SubComponent =>

  import global._

  def isConstructor(ddef: DefDef) = {
    ddef.name.toString.trim() == "<init>" && !ddef.mods.hasAccessorFlag
  }

  def isSupportedConstructor(cdef: ClassDef) = {
    val constructorsCount = cdef.impl.body.count {
      //        case ddef: DefDef if (ddef.name.toString.trim() != "$init$" && !ddef.mods.hasAccessorFlag && ddef.name.toString.trim() == "<init>") => true
      case ddef: DefDef if (isConstructor(ddef)) => true
      case _ => false
    }

    constructorsCount <= 1
  }

  def findConstructor(cdef: ClassDef): Option[DefDef] = {
    cdef.impl.body.collectFirst {
      case ddef: DefDef if (isConstructor(ddef)) => ddef
    }
  }
}