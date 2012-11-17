package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction

trait FunctionConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toParameterNames(params: List[ValDef], scopedVar: ScopedVariables): Map[String, String] = {
    params.map {
      param =>
        scopedVar.put(param.name.toString, param.symbol) -> PrimitiveTypes.toPrimitiveType(param.symbol.tpe.toString)
    }.toMap
  }

  def convertFunction(fun: Function, scopedVars: ScopedVariables) = {
    val newScope = new ScopedVariables(scopedVars)

    NoMoreScriptJsFunction(None, toParameterNames(fun.vparams, newScope).toMap, toTree(fun.body, newScope, !fun.tpe.resultType.toString.endsWith("=> Unit")))
  }
}
