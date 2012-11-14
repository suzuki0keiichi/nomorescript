package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction

trait ApplyImplicitViewConverter extends ConverterBase with FunctionConverter {
  self: SubComponent =>

  import global._

  def toAnonFun(cdef: ClassDef, scopedVars: ScopedVariables) = {
    cdef.impl.body.collectFirst {
      case ddef: DefDef if (ddef.tpt.toString == "Unit") => ddef
    } match {
      case Some(ddef) =>
        val newScopedVars = new ScopedVariables(scopedVars)
        NoMoreScriptJsFunction(None, toParameterNames(ddef.vparamss(0), newScopedVars).toMap, toTree(ddef.rhs, newScopedVars, ddef.tpt.toString != "Unit"))

      case _ =>
        NoMoreScriptEmpty()
    }
  }

}