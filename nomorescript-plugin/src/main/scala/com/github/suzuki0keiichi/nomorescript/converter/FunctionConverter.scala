package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global

trait FunctionConverter {
  self: SubComponent =>

  import global._

  def toParameterNames(params: List[ValDef], scopedVar: ScopedVariables): Map[String, String] = {
    params.map {
      param =>
        scopedVar.put(param.name.toString, param.symbol) -> PrimitiveTypes.toPrimitiveType(param.symbol.tpe.toString)
    }.toMap
  }
}