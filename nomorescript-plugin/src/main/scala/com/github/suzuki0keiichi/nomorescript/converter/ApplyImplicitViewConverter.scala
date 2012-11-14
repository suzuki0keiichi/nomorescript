package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction

trait ApplyImplicitViewConverter extends ConverterBase with ConvertErrorReporter with FunctionConverter {
  self: SubComponent =>

  import global._

  private def toAnonFun(cdef: ClassDef, scopedVars: ScopedVariables) = {
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

  def convertApplyImplicitView(aplyImplicit: ApplyImplicitView, scopedVars: ScopedVariables) = {
    aplyImplicit.args(0) match {
      case fun: Function =>
        val newScope = new ScopedVariables(scopedVars)
        NoMoreScriptJsFunction(Some(fun.vparams.head.name.toString()), toParameterNames(fun.vparams.tail, newScope).toMap, toTree(fun.body, newScope, !fun.tpe.resultType.toString.endsWith("=> Unit")))

      case Block(_, Typed(Apply(Select(New(className), _), _), _)) if (className.toString.startsWith("anonymous class ")) =>
        findClass(className.toString.substring("anonymous class ".length)) match {
          case Some(cdef) => toAnonFun(cdef, scopedVars)

          case _ =>
            addError(aplyImplicit.pos, "unknown error")
            NoMoreScriptEmpty()
        }

      case _ =>
        NoMoreScriptEmpty()
    }
  }
}
