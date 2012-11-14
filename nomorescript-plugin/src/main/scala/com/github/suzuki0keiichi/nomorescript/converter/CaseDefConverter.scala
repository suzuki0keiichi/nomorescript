package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptInstanceOf
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptVal

trait CaseDefConverter extends ConverterBase {
  self: SubComponent =>

  import global._

  def toCase(caseDef: CaseDef, scopedVars: ScopedVariables, returnValue: Boolean) = {
    caseDef.pat match {
      case b: Bind =>
        b.body match {
          case t: Typed =>
            NoMoreScriptIf(
              NoMoreScriptInstanceOf(NoMoreScriptIdent("__match_target__", false), t.tpt.toString),
              NoMoreScriptTrees(List(
                NoMoreScriptVal(b.name.toString, NoMoreScriptIdent("__match_target__", false), false),
                toTree(caseDef.body, scopedVars, returnValue)), false),
              NoMoreScriptEmpty())
        }

      case i: Ident =>
        NoMoreScriptIf(NoMoreScriptEmpty(), toTree(caseDef.body, scopedVars, returnValue), NoMoreScriptEmpty())
    }
    //      NoMoreScriptTrees(
    //        List(toTree(caseDef.pat, false, None), toTree(caseDef.guard, false, None), toTree(caseDef.body, returnValue, None)), false)
  }
}