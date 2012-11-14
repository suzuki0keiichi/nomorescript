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

trait TreeConverter extends ConverterBase with ThisConverter with PackageDefConverter with ClassDefConverter with ValDefConverter with SelectConverter
  with IdentConverter with NewConverter with ApplyImplicitViewConverter with ApplyConverter with TypeApplyConverter
  with DefDefConverter with FunctionConverter with CaseDefConverter with SuperConverter with BlockConverter with TryConverter 
  with LiteralConverter {
  self: SubComponent =>

  import global._

  private def isJsFunction(aplyImplicit: ApplyImplicitView) = {
    aplyImplicit.fun.toString.startsWith("com.github.suzuki0keiichi.nomorescript.bridge.bridge.toJsFunction")
  }

  private def isNormalFunction(ddef: DefDef) = {
      ddef.name.toString != "<init>" && ddef.name.toString != "$init$" && !ddef.mods.hasAccessorFlag
  }

  def toTree(tree: Tree, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String] = Map.empty[String, String]): NoMoreScriptTree = {
    tree match {
      case ths: This => toThis(ths, returnValue)
      case pdef: PackageDef => toPackage(pdef, scopedVars)
      case cdef: ClassDef if (!isAnonFun(cdef.name.toString)) => convertClassDef(cdef, scopedVars)
      case vdef: ValDef => toVal(vdef, scopedVars, memberNames)
      case select: Select => toSelect(select, scopedVars, returnValue)
      case ident: Ident => convertIdent(ident, scopedVars, returnValue)
      case nw: New => convertNew(nw, scopedVars, returnValue)
      case fun: Function => convertFunction(fun, scopedVars)
      case sper: Super if (!BaseClasses.isBaseClass(sper.symbol.superClass.name.toString)) => convertSuper(sper, returnValue)
      case block: Block => convertBlock(block, scopedVars, returnValue, memberNames)
      case Try(block, catches, finalizer) => convertTry(tree, block, catches, finalizer, scopedVars, returnValue, memberNames)
      case aplyImplicit: ApplyImplicitView if (isJsFunction(aplyImplicit)) => convertApplyImplicitView(aplyImplicit, scopedVars)
      case aply: Apply => toApply(aply, scopedVars, returnValue)
      case ddef: DefDef if (isNormalFunction(ddef)) => convertDefDef(ddef, scopedVars)
      case ifBlock: If => NoMoreScriptIf(toTree(ifBlock.cond, scopedVars, false), toTree(ifBlock.thenp, scopedVars, returnValue), toTree(ifBlock.elsep, scopedVars, returnValue))
      case literal: Literal => convertLiteral(literal, returnValue)
      case t: Throw => NoMoreScriptThrow(toTree(t.expr, scopedVars, false))

      case m: Match =>
        // NoMoreScriptTrees(List(toTree(m.selector, false, None)) ::: m.cases.map(toCase(_, returnValue)), true)
        addError(m.pos, "pattern match is not supported")
        NoMoreScriptEmpty()

      case t: Tree => NoMoreScriptEmpty()
    }
  }

}
