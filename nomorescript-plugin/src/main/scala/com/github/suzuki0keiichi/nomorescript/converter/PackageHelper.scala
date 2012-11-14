package com.github.suzuki0keiichi.nomorescript.converter

import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent

trait PackageHelper {
  self: SubComponent =>

  import global._

  def getPackageName(symbol: Symbol, child: String): Option[String] = {
    if (symbol.isRoot || symbol.name.toString == "<empty>") {
      if (child == null) None else Some(child)
    } else {
      getPackageName(symbol.owner, if (child == null) symbol.name.toString else symbol.name.toString + "." + child)
    }
  }
}