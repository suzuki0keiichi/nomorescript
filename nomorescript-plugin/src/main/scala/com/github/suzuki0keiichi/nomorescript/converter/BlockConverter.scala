package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees

trait BlockConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def convertBlock(block: Block, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String]) = {
    val children = block.stats.map(toTree(_, scopedVars, false, memberNames)).toList ::: List(toTree(block.expr, scopedVars, returnValue, memberNames))

    NoMoreScriptTrees(children, false)
  }
}
