package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptFor

trait TypeApplyConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toTypeApply(aply: TypeApply, args: List[Tree], scopedVars: ScopedVariables, returnValue: Boolean) = {
    aply match {
      case TypeApply(
      Select(
      Apply(
      Select(
      Apply(
      Select(
      Select(
      This(thisName), predefName),
      intWrapperName),
      List(begin)),
      untilName),
      List(end)),
      foreachName),
      List(tpe))
        if (thisName.toString() == "scala" &&
          predefName.toString() == "Predef" &&
          intWrapperName.toString() == "intWrapper" &&
          untilName.toString() == "until" &&
          foreachName.toString() == "foreach" &&
          tpe.toString() == "Unit" &&
          args.size == 1) =>

        args match {
          case List(Function(List(param), body)) =>
            NoMoreScriptFor(begin.toString(), end.toString(), "1", scopedVars.put(param.name.toString(), param.symbol),
              toTree(body, scopedVars, false))

          case _ => NoMoreScriptEmpty()
        }

      case _ =>
        aply.fun match {
          case select: Select if (select.name.toString == "$asInstanceOf") =>
            toTree(select.qualifier, scopedVars, returnValue)

          case _ =>
            NoMoreScriptEmpty()
        }
    }
  }

}