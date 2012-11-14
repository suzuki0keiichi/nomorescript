package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent

import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptNamespace
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees

trait PackageDefConverter extends ConverterBase with PackageHelper {
  self: SubComponent =>

  import global._

  def toPackage(pdef: PackageDef, scopedVars: ScopedVariables) = {
    val name = pdef.name.toString.trim()

    getPackageName(pdef.symbol, null) match {
      case Some(name) =>
        NoMoreScriptNamespace(name, pdef.stats.map(toTree(_, scopedVars, false)))
      case None =>
        NoMoreScriptTrees(pdef.stats.map(toTree(_, scopedVars, false)), false)
    }
  }


}