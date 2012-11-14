package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTraitDef
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptDef

trait DefDefConverter extends ConverterBase with PackageHelper with AnnotationHelper with FunctionConverter {
  self: SubComponent =>

  import global._

  private def isGlobalClass(ddef: DefDef): Boolean = {
    isGlobalClass(ddef.symbol.owner.tpe.typeSymbol)
  }

  def convertDefDef(ddef: DefDef, scopedVars: ScopedVariables) = {
    val className = getPackageName(ddef.symbol.owner.owner, null) match {
      case Some(packageName) => packageName + "." + ddef.symbol.owner.name.toString
      case _ => ddef.symbol.owner.name.toString
    }

    if (isGlobalClass(ddef)) {
      if (ddef.name.toString == ddef.tpe.typeSymbol.enclClass.toString) {
        // コンストラクタなので関数ではなくそのまま処理を記述
        toTree(ddef.rhs, scopedVars, false)
      } else {
        val newScope = new ScopedVariables(scopedVars)

        // global上の関数
        NoMoreScriptDef(
          ddef.name.toString,
          ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
          toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
          None)
      }
    } else if (ddef.symbol.enclClass.isTrait) {
      val newScope = new ScopedVariables(scopedVars)

      // traitの関数
      NoMoreScriptTraitDef(
        ddef.name.toString,
        ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
        toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
        className)
    } else {
      val newScope = new ScopedVariables(scopedVars)

      // 通常のクラス関数
      NoMoreScriptDef(
        ddef.name.toString,
        ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
        toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
        Some(className))
    }
  }
}
