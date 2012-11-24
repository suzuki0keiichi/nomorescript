package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptOperator
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSetter
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptThis
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptInstanceOf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptApply

trait ApplyConverter extends ConverterBase with SelectConverter with TypeApplyConverter {
  self: SubComponent =>

  import global._

  def toApply(aply: Apply, scopedVars: ScopedVariables, returnValue: Boolean) = {
    aply.fun match {
      case select: Select if (select.name.toString == "$isInstanceOf") =>
        NoMoreScriptInstanceOf(toTree(select.qualifier, scopedVars, returnValue), aply.args(0).toString)

      case typeApply: TypeApply =>
        toTypeApply(typeApply, aply.args, scopedVars, returnValue)

      case select: Select =>
        toSetter(aply) match {
          case Some(setter) =>
            NoMoreScriptSetter(setter, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false))

          case None =>
            toGetter(aply, scopedVars, returnValue) match {
              case Some(getter) => getter
              case None =>
                toOperator(aply) match {
                  case Some(operator) =>
                    NoMoreScriptOperator(operator, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false), returnValue)

                  case None =>
                    val paramForSuperMethod = select.symbol match {
                      case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
                        List(NoMoreScriptThis(false))
                      case _ => Nil
                    }

                    val params = paramForSuperMethod ++ aply.args.map(toTree(_, scopedVars, false))

                    NoMoreScriptApply(toSelect(select, scopedVars, false), params, returnValue, isArrayApply(aply))
                }
            }
        }

      case _ =>
        val params = aply.args.map(toTree(_, scopedVars, false))

        NoMoreScriptApply(toTree(aply.fun, scopedVars, false), params, returnValue, isArrayApply(aply))
    }
  }

  def toOperator(aply: Apply) = {
    aply.fun.asInstanceOf[Select].name.toString match {
      case "$plus" => Some("+")
      case "$minus" => Some("-")
      case "$times" => Some("*")
      case "$div" => Some("/")
      case "$eq$eq" => Some("==")
      case "$bang$eq" => Some("!=")
      case "$greater" => Some(">")
      case "$less" => Some("<")
      case "$percent" => Some("%")
      case _ => None
    }
  }

  val setterMatcher = "([a-zA-Z0-9_]+)_\\$eq".r

  def toSetter(aply: Apply) = {
    aply.fun.asInstanceOf[Select].name.toString match {
      case setterMatcher(name) => Some(name)
      case a => None
    }
  }

  def toGetter(aply: Apply, scopedVars: ScopedVariables, returnValue: Boolean) = {
    aply.fun match {
      case select: Select if (aply.symbol.hasAccessorFlag) => Some(toSelect(select, scopedVars, returnValue))
      case _ => None
    }
  }
  
  def isArrayApply(aply: Apply): Boolean = {
    val COLLECTION_NAMES = List("Array", "Map")

    aply.fun match {
      case Select(qualifier, name) if (name.toString == "apply" && COLLECTION_NAMES.exists(name => qualifier.tpe.toString.indexOf(name + "[") != -1)) =>
        true

      case Select(Select(Ident(packageName), companionName), name) if (name.toString == "apply" && COLLECTION_NAMES.exists(_ == companionName.toString) && packageName.toString == "scala") =>
        true

      case _ =>
        false
    }
  }
}