package com.github.suzuki0keiichi.nomorescript.converter
import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent

trait TraitHelper {
  self: SubComponent =>

  import global._

  def getSuperTraitsWithParent(parent: Symbol): List[Symbol] = {
    val traits = parent.tpe.parents.collect {
      case t: UniqueTypeRef if (!BaseClasses.isBaseClass(t.toString) && !t.typeSymbol.isTrait) =>
        getSuperTraitsWithParent(t.typeSymbol)

      case t: UniqueTypeRef if (!BaseClasses.isBaseClass(t.toString) && t.typeSymbol.isTrait) =>
        getSuperTraitsWithParent(t.typeSymbol) ::: List(t.typeSymbol)
    }

    traits.flatten.distinct
  }

  def getAllSuperTraits(parent: Symbol, excludes: List[Symbol]): List[Symbol] = {
    val traits = parent.tpe.parents.collect {
      case t: UniqueTypeRef if (!BaseClasses.isBaseClass(t.toString) && t.typeSymbol.isTrait && !excludes.contains(t.typeSymbol)) =>
        getSuperTraitsWithParent(t.typeSymbol) ::: List(t.typeSymbol)
    }

    traits.flatten.distinct
  }
}