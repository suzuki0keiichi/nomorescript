package com.github.suzuki0keiichi.nomorescript.converter

import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTree

trait ConverterBase {
  self: SubComponent =>

  import global._

  def toTree(tree: Tree, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String] = Map.empty[String, String]): NoMoreScriptTree

  def findClass(name: String): Option[ClassDef]
}