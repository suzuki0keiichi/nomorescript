package com.github.suzuki0keiichi.nomorescript.converter

import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent

trait SuperConverter extends ConverterBase with PackageHelper {
  self: SubComponent =>

  import global._

  def convertSuper(sper: Super, returnValue: Boolean) = {
    val parent = sper.symbol.tpe.parents.collectFirst {
      case ref: UniqueTypeRef if (!ref.typeSymbol.isTrait) => ref.typeSymbol
    }

    NoMoreScriptIdent(getPackageName(parent.get, null).get, returnValue)
  }
}
