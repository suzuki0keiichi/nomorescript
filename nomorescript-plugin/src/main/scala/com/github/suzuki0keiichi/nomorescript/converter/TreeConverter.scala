package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.Global
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTry
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptLiteral
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptThrow
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTree
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTry
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptCases
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptJsFunction

trait TreeConverter extends ThisConverter with PackageConverter with ClassConverter with ValDefConverter with SelectConverter
  with IdentConverter with NewConverter with ApplyImplicitViewConverter with ApplyConverter with TypeApplyConverter
  with DefDefConverter with FunctionConverter with CaseDefConverter {
  self: SubComponent =>

  import global._
  
  def findClass(name: String): Option[ClassDef]

  def toTree(tree: Tree, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String] = Map.empty[String, String]): NoMoreScriptTree = {
    tree match {
      case ths: This => toThis(ths, returnValue)
      case pdef: PackageDef => toPackage(pdef, scopedVars)
      case cdef: ClassDef if (!isAnonFun(cdef.name.toString)) => toClass(cdef, scopedVars)
      case vdef: ValDef => toVal(vdef, scopedVars, memberNames)
      case select: Select => toSelect(select, scopedVars, returnValue)
      case ident: Ident => toIdent(ident, scopedVars, returnValue)
      case nw: New => toNew(nw, scopedVars, returnValue)
      case fun: Function =>
        val newScope = new ScopedVariables(scopedVars)
        NoMoreScriptJsFunction(None, toParameterNames(fun.vparams, newScope).toMap, toTree(fun.body, newScope, !fun.tpe.resultType.toString.endsWith("=> Unit")))

      case sper: Super if (!BaseClasses.isBaseClass(sper.symbol.superClass.name.toString)) =>
        val parent = sper.symbol.tpe.parents.collectFirst {
          case ref: UniqueTypeRef if (!ref.typeSymbol.isTrait) => ref.typeSymbol
        }

        NoMoreScriptIdent(getPackageName(parent.get, null).get, returnValue)

      case aplyImplicit: ApplyImplicitView if (aplyImplicit.fun.toString.startsWith("com.github.suzuki0keiichi.nomorescript.bridge.bridge.toJsFunction")) =>
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

      case aply: Apply => toApply(aply, scopedVars, returnValue)

      case ddef: DefDef if (ddef.name.toString != "<init>" && ddef.name.toString != "$init$" && !ddef.mods.hasAccessorFlag) =>
        toDef(ddef, scopedVars)

      case block: Block =>
        val children = block.stats.map(toTree(_, scopedVars, false, memberNames)).toList :::
          List(toTree(block.expr, scopedVars, returnValue, memberNames))

        NoMoreScriptTrees(children, false)

      case Try(block, catches, finalizer) =>
        NoMoreScriptTry(
          toTree(block, scopedVars, returnValue, memberNames),
          NoMoreScriptCases(catches.map(toCase(_, scopedVars, returnValue))),
          toTree(finalizer, scopedVars, false, memberNames))

      case ifBlock: If =>
        NoMoreScriptIf(toTree(ifBlock.cond, scopedVars, false), toTree(ifBlock.thenp, scopedVars, returnValue), toTree(ifBlock.elsep, scopedVars, returnValue))

      case literal: Literal =>
        literal.value.value match {
          case s: String => NoMoreScriptLiteral("\"" + literal.value.value.toString + "\"", returnValue)
          case _ =>
            if (literal.value.value == null) {
              NoMoreScriptLiteral("null", returnValue)
            } else {
              NoMoreScriptLiteral(literal.value.value.toString, returnValue)
            }
        }

      case t: Throw =>
        NoMoreScriptThrow(toTree(t.expr, scopedVars, false))

      case m: Match =>
        // NoMoreScriptTrees(List(toTree(m.selector, false, None)) ::: m.cases.map(toCase(_, returnValue)), true)
        addError(m.pos, "pattern match is not supported")
        NoMoreScriptEmpty()

      case t: Tree =>
        NoMoreScriptEmpty()
    }
  }

}