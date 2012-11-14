package com.github.suzuki0keiichi.nomorescript.converter

import com.github.suzuki0keiichi.nomorescript.annotation.global
import scala.tools.nsc.SubComponent
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptEmpty
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptClass
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrait
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTree
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptTrees
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptApply
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptConstructor
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptSelect
import com.github.suzuki0keiichi.nomorescript.trees.NoMoreScriptIdent

trait ClassConverter extends ConverterBase with PackageHelper with AnnotationHelper with ConstructorHelper with TraitHelper with ConvertErrorReporter
  with ValDefConverter with FunctionConverter {
  self: SubComponent =>

  import global._

  def toClass(cdef: ClassDef, scopedVars: ScopedVariables): NoMoreScriptTree = {
    if (cdef.mods.hasModuleFlag) {
      toModule(cdef, scopedVars)
    } else {
      if (!isSupportedConstructor(cdef)) {
        addError(cdef.pos, "multi constructor is not supported")

        return NoMoreScriptEmpty()
      }

      if (haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.mock")) {
        return NoMoreScriptEmpty()
      }

      val name = cdef.name.toString.trim()
      val isTrait = cdef.impl.tpe.typeSymbol.isTrait

      val parent = cdef.impl.parents.collectFirst {
        case t: Ident if (!BaseClasses.isBaseClass(t.toString) && !t.tpe.typeSymbol.isTrait) => t
      }

      val parentName: Option[String] = parent.map {
        t =>
          getPackageName(t.symbol.owner, null) match {
            case Some(packageName) => packageName + "." + t.toString
            case _ => t.toString
          }
      }

      val superTraitsWithParent = parent match {
        case Some(t) => getSuperTraitsWithParent(t.tpe.typeSymbol)
        case None => Nil
      }

      val traits = getAllSuperTraits(cdef.impl.tpe.typeSymbol, superTraitsWithParent)

      val traitNames: List[String] = traits.map(getPackageName(_, null).get)

      val implementedTraitMethods: Map[String, List[String]] = traits.collect {
        case t: Symbol =>
          getPackageName(t, null).get -> t.tpe.members.collect {
            case m: MethodSymbol if (t == m.enclClass.tpe.typeSymbol && m.name.toString.indexOf("$") == -1) => m.name.toString
          }
      }.toMap

      val members: Map[String, String] = cdef.impl.body.collect {
        case vdef: ValDef =>
          vdef.name.toString.trim -> toPrimitiveType(vdef.tpt.toString)
      }.toMap

      val namespace = getPackageName(cdef.symbol.owner, null)

      if (isTrait) {
        NoMoreScriptTrait(name, namespace, toConstructor(cdef, scopedVars, Nil),
          cdef.impl.body.collect {
            case ddef: DefDef if (ddef.name.toString.indexOf("$") == -1) =>
              ddef.name.toString -> toTree(ddef, scopedVars, false)
          }.toMap)
      } else {
        NoMoreScriptClass(name, namespace, toConstructor(cdef, scopedVars, traitNames), parentName, traitNames,
          implementedTraitMethods,
          members,
          cdef.impl.body.collect {
            case ddef: DefDef => ddef.name.toString -> toTree(ddef, scopedVars, false)
          }.toMap)
      }
    }
  }

  def toModule(cdef: ClassDef, scopedVars: ScopedVariables) = {
    val global = haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.global")
    val mock = haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.mock")

    if (mock) {
      NoMoreScriptEmpty()
    } else if (global) {
      NoMoreScriptTrees(cdef.impl.body.map(toTree(_, scopedVars, false)), true)
    } else {
      addError(cdef.pos, "singleton class is not supported")
      NoMoreScriptEmpty()
    }
  }

  def toConstructor(cdef: ClassDef, scopedVars: ScopedVariables, superTraitNames: List[String]) = {
    val newScopedVars = new ScopedVariables(scopedVars)
    val params: Map[String, String] = cdef.impl.body.collectFirst {
      case ddef: DefDef if (ddef.name.toString.trim() == "<init>") => toParameterNames(ddef.vparamss.head, newScopedVars)
    }.getOrElse(Map[String, String]())

    val memberNames: Map[String, String] = cdef.impl.body.collect {
      case vdef: ValDef =>
        vdef.name.toString.trim() -> toPrimitiveType(vdef.tpt.toString)
    }.toMap

    val callSuperClass = cdef.impl.body.collectFirst {
      case ddef: DefDef if (ddef.name.toString == "<init>" && !BaseClasses.isBaseClass(ddef.symbol.enclClass.superClass.name.toString)) =>
        ddef.rhs match {
          case b: Block => b.stats.collect {
            case aply: Apply =>
              toTree(aply, newScopedVars, false, memberNames) match {
                case aply: NoMoreScriptApply => aply.toSuperConstructorApply()
                case _ => NoMoreScriptEmpty()
              }
          }

          case _ => Nil
        }
    }.getOrElse(Nil)

    val callSuperTraits = superTraitNames.map {
      name =>
        NoMoreScriptApply(NoMoreScriptSelect("call", NoMoreScriptIdent(name, false)), List(NoMoreScriptIdent("this", false)), false, false)
    }

    val bodies = cdef.impl.body.filter {
      case ddef: DefDef => false
      case tree: Tree => true
    }.map(toTree(_, newScopedVars, false, memberNames))

    val name = getPackageName(cdef.symbol.owner, null) match {
      case Some(packageName) => packageName + "." + cdef.name.toString.trim()
      case None => cdef.name.toString.trim()
    }

    NoMoreScriptConstructor(name, params, memberNames, callSuperClass ++ callSuperTraits ++ bodies)
  }

  def isAnonFun(name: String): Boolean = {
    name.startsWith("$anonfun$")
  }
}