package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTry
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptCases

trait TryConverter extends ConverterBase with CaseDefConverter {
  self: SubComponent =>

  import global._

  def convertTry(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String]) = {
    NoMoreScriptTry(
      toTree(block, scopedVars, returnValue, memberNames),
      NoMoreScriptCases(catches.map(toCase(_, scopedVars, returnValue))),
      toTree(finalizer, scopedVars, false, memberNames))
  }
}
