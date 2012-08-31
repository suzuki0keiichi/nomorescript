package com.github.suzuki0keiichi.nomorescript.plugin

import java.io.IOException

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

import com.github.suzuki0keiichi.nomorescript.trees._

class NoMoreScriptPluginComponent(val global: Global, parent: NoMoreScriptPlugin) extends PluginComponent {

  import global._

  val runsAfter: List[String] = List("refchecks")

  val phaseName: String = "scala to javascript convert phase"

  def newPhase(prev: Phase): Phase = new NoMoreScriptPhase(prev)

  class NoMoreScriptPhase(prev: Phase) extends StdPhase(prev) {

    override def name: String = phaseName

    val BASE_CLASSES = List("java.lang.Object", "ScalaObject", "Product", "Serializable", "Object")
    val PRIMITIVE_TYPES = Map("Int" -> "number", "String" -> "string", "Any" -> "Object")
    val localUnit = new ThreadLocal[(CompilationUnit, Boolean)]

    private def addError(pos: Position, message: String) {
      localUnit.get()._1.error(pos, message)
      localUnit.set((localUnit.get()._1, true))
    }

    def apply(unit: CompilationUnit) {
      localUnit.set((unit, false))

      val currentDir = new java.io.File(".")

      unit.body match {
        case pdef: PackageDef =>
          val js = toPackage(pdef, None).toJs(false)

          if (!localUnit.get._2) {
            val writer = createWriter(currentDir, unit)

            try {
              js.foreach(writer.println(_))
              writer.flush()
            } catch {
              case e: IOException => addError(NoPosition, e.getMessage)
            } finally {
              writer.close()
            }
          }

        case t: Tree =>
          unit.error(t.pos, "not supported " + t.getClass)
      }
    }

    private def createWriter(currentDir: java.io.File, unit: CompilationUnit) = {
      val file = unit.source.file.file
      val relativePath =
        if (file.getParent.startsWith(parent.srcRootDir)) {
          file.getParent.substring(parent.srcRootDir.length())
        } else {
          file.getParent
        }

      val outputDirRoot = {
        val file = new java.io.File(parent.outputDir)

        if (file.isAbsolute) {
          file.getAbsolutePath
        } else {
          currentDir.getAbsolutePath + "/" + file.getPath
        }
      }

      val outputDir = new java.io.File(outputDirRoot + "/" + relativePath)

      outputDir.mkdirs()

      new java.io.PrintWriter(outputDir.getPath + "/" + file.getName.replaceAll(".scala", "") + ".js", "UTF-8")
    }

    def toPackage(pdef: PackageDef, parentPackageName: Option[String]) = {
      val name = pdef.name.toString.trim()
      if (NoMoreScriptNamespace.isEmpty(name)) {
        NoMoreScriptTrees(pdef.stats.map(toTree(_, false, None)), false)
      } else {
        val namespace = parentPackageName.map(_ + ".").getOrElse("") + pdef.pid.toString

        NoMoreScriptNamespace(namespace, pdef.stats.map(toTree(_, false, Some(namespace))))
      }
    }

    def getPackageName(symbol: Symbol, child: String): Option[String] = {
      if (symbol.isRoot || symbol.name.toString == "<empty>") {
        if (child == null) None else Some(child)
      } else {
        getPackageName(symbol.owner, if (child == null) symbol.name.toString else child + "." + symbol.name.toString)
      }
    }

    def toClass(cdef: ClassDef, namespace: Option[String]): NoMoreScriptTree = {
      if (cdef.mods.hasModuleFlag) {
        toModule(cdef)
      } else {
        if (!isSupportedConstructor(cdef)) {
          addError(cdef.pos, "multi constructor is not supported")

          return NoMoreScriptTree()
        }

        if (haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.mock")) {
          return NoMoreScriptTree()
        }

        val name = cdef.name.toString.trim()
        val isTrait = cdef.impl.tpe.typeSymbol.isTrait

        val parent: Option[String] = cdef.impl.parents.collectFirst {
          case t: Ident if (!BASE_CLASSES.contains(t.toString) && !t.tpe.typeSymbol.isTrait) =>
            getPackageName(t.symbol.owner, null) match {
              case Some(packageName) => packageName + "." + t.toString
              case _ => t.toString
            }
        }

        // TODO:これだと親の親が取れない(JsDocでinterfaceは継承できないと思うので)
        val traits: List[String] = cdef.impl.parents.collect {
          case t: TypeTree if (!BASE_CLASSES.contains(t.toString) && t.tpe.typeSymbol.isTrait) => t.toString
        }

        // TODO:これだとまだ親の親で実装されている関数が取れない
        val implementedTraitMethods: Map[String, List[String]] =
          cdef.impl.parents.collect {
            case t: TypeTree if (!BASE_CLASSES.contains(t.toString) && t.tpe.typeSymbol.isTrait) =>
              t.toString -> t.tpe.members.collect {
                case m: MethodSymbol if (t.toString == m.enclClass.tpe.toString && !m.name.toString.startsWith("$")) => m.name.toString
              }
          }.toMap

        val members: Map[String, String] = cdef.impl.body.collect {
          case ddef: DefDef if (ddef.mods.hasAccessorFlag && ddef.tpt.toString != "Unit") =>
            ddef.name.toString.trim() -> toPrimitiveType(ddef.tpt.toString)
        }.toMap

        val children = Map[String, NoMoreScriptTree]()

        if (isTrait) {
          NoMoreScriptTrait(name, namespace, toConstructor(cdef, namespace),
            cdef.impl.body.collect {
              case ddef: DefDef => ddef.name.toString -> toTree(ddef, false, namespace)
            }.toMap)
        } else {
          NoMoreScriptClass(name, namespace, toConstructor(cdef, namespace), parent, traits,
            implementedTraitMethods,
            members,
            cdef.impl.body.collect {
              case ddef: DefDef => ddef.name.toString -> toTree(ddef, false, namespace)
            }.toMap)
        }
      }
    }

    def haveAnnotation(cdef: ClassDef, name: String) = {
      cdef.symbol.annotations.exists {
        a: AnnotationInfo => a.toString == name
      }
    }

    def toModule(cdef: ClassDef) = {
      val global = haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.global")
      val mock = haveAnnotation(cdef, "com.github.suzuki0keiichi.nomorescript.annotation.mock")

      if (mock) {
        NoMoreScriptTree()
      } else if (global) {
        NoMoreScriptTrees(cdef.impl.body.map(toTree(_, false, Some(cdef.name.toString))), true)
      } else {
        addError(cdef.pos, "singleton class is not supported")
        NoMoreScriptTree()
      }
    }

    def isConstructor(ddef: DefDef) = {
      ddef.name.toString.trim() == "<init>" && !ddef.mods.hasAccessorFlag
    }

    def isSupportedConstructor(cdef: ClassDef) = {
      val constructorsCount = cdef.impl.body.count {
        //        case ddef: DefDef if (ddef.name.toString.trim() != "$init$" && !ddef.mods.hasAccessorFlag && ddef.name.toString.trim() == "<init>") => true
        case ddef: DefDef if (isConstructor(ddef)) => true
        case _ => false
      }

      constructorsCount <= 1
    }

    def findConstructor(cdef: ClassDef): Option[DefDef] = {
      cdef.impl.body.collectFirst {
        case ddef: DefDef if (isConstructor(ddef)) => ddef
      }
    }

    def toParameterNames(params: List[ValDef]): Map[String, String] = {
      params.map(param => param.name.toString -> toPrimitiveType(param.symbol.tpe.toString)).toMap
    }

    def toConstructor(cdef: ClassDef, namespace: Option[String]) = {
      val params: Map[String, String] = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString.trim() == "<init>") => toParameterNames(ddef.vparamss.head)
      }.getOrElse(Map[String, String]())

      val memberNames: Map[String, String] = cdef.impl.body.collect {
        case ddef: DefDef if (ddef.mods.hasAccessorFlag && ddef.tpt.toString != "Unit") =>
          ddef.name.toString.trim() -> toPrimitiveType(ddef.tpt.toString)
      }.toMap

      val callSuperClass = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString == "<init>" && !BASE_CLASSES.contains(ddef.symbol.enclClass.superClass.name.toString)) =>
          ddef.rhs match {
            case b: Block => b.stats.collect {
              case aply: Apply =>
                toTree(aply, false, namespace, memberNames) match {
                  case aply: NoMoreScriptApply => aply.toSuperConstructorApply()
                  case _ => NoMoreScriptTree()
                }
            }

            case _ => Nil
          }
      }.getOrElse(Nil)

      val bodies = cdef.impl.body.filter {
        case ddef: DefDef => false
        case tree: Tree => true
      }.map(toTree(_, false, namespace, memberNames))

      val name = namespace match {
        case Some(packageName) => packageName + "." + cdef.name.toString.trim()
        case None => cdef.name.toString.trim()
      }

      NoMoreScriptConstructor(name, params, memberNames, callSuperClass ::: bodies)
    }

    def isAnonFun(ddef: DefDef): Boolean = {
      isAnonFun(ddef.name.toString.trim)
    }

    def isAnonFun(name: String): Boolean = {
      name.startsWith("$anonfun$")
    }

    def toAnonFun(cdef: ClassDef) = {
      cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.tpt.toString == "Unit") => ddef
      } match {
        case Some(ddef) => NoMoreScriptJsFunction(toParameterNames(ddef.vparamss(0)).toMap, toTree(ddef.rhs, ddef.tpt.toString != "Unit"))
        case _ => NoMoreScriptTree()
      }
    }

    def toVal(vdef: ValDef, memberNames: Map[String, String]) = {
      val name = vdef.name.toString.trim()
      val isMember = memberNames.contains(name)

      if (vdef.rhs.toString.trim() == "<empty>") {
        if (isMember) {
          NoMoreScriptVal(name, NoMoreScriptIdent(name, false), isMember)
        } else {
          NoMoreScriptTree()
        }
      } else {
        NoMoreScriptVal(name, toTree(vdef.rhs, false), isMember)
      }
    }

    def toSelect(select: Select, returnValue: Boolean) = {
      if (select.name.toString == "package") {
        toTree(select.qualifier, returnValue)
      } else {
        NoMoreScriptSelect(select.name.toString, toTree(select.qualifier, returnValue))
      }
    }

    val setterMatcher = "([a-zA-Z0-9_]+)_\\$eq".r

    def toSetter(aply: Apply) = {
      aply.fun.asInstanceOf[Select].name.toString match {
        case setterMatcher(name) => Some(name)
        case a => None
      }
    }

    def toGetter(aply: Apply, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (aply.symbol.hasAccessorFlag) => Some(toSelect(select, returnValue))
        case _ => None
      }
    }

    def toOperator(aply: Apply) = {
      aply.fun.asInstanceOf[Select].name.toString match {
        case "$plus" => Some("+")
        case "$minus" => Some("-")
        case "$times" => Some("*")
        case "$div" => Some("/")
        case "$eq$eq" => Some("==")
        case "$bang$eq" => Some("!=")
        case "$greater" => Some(">")
        case "$less" => Some("<")
        case "$percent" => Some("%")
        case _ => None
      }
    }

    val COLLECTION_NAMES = List("Array", "Map")

    def isArrayApply(aply: Apply): Boolean = {
      aply.fun match {
        case Select(qualifier, name) if (name.toString == "apply" && COLLECTION_NAMES.exists(name => qualifier.tpe.toString.indexOf(name + "[") != -1)) =>
          true

        case Select(Select(Ident(packageName), companionName), name) if (name.toString == "apply" && COLLECTION_NAMES.exists(_ == companionName.toString) && packageName.toString == "scala") =>
          true

        case _ =>
          false
      }
    }

    def toApply(aply: Apply, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (select.name.toString == "$isInstanceOf") =>
          NoMoreScriptInstanceOf(toTree(select.qualifier, returnValue), aply.args(0).toString)

        case typeApply: TypeApply =>
          toTypeApply(typeApply, returnValue)

        case select: Select =>
          toSetter(aply) match {
            case Some(setter) =>
              NoMoreScriptSetter(setter, toTree(aply.fun.asInstanceOf[Select].qualifier, false, None), toTree(aply.args(0), false))

            case None =>
              toGetter(aply, returnValue) match {
                case Some(getter) => getter
                case None =>
                  toOperator(aply) match {
                    case Some(operator) =>
                      NoMoreScriptOperator(operator, toTree(aply.fun.asInstanceOf[Select].qualifier, false, None), toTree(aply.args(0), false), returnValue)

                    case None =>
                      val params = aply.args.map(toTree(_, false, None))

                      NoMoreScriptApply(toSelect(select, false), params, returnValue, isArrayApply(aply))
                  }
              }
          }

        case _ =>
          val params = aply.args.map(toTree(_, false, None))

          NoMoreScriptApply(toTree(aply.fun, false, None), params, returnValue, isArrayApply(aply))
      }
    }

    def toTypeApply(aply: TypeApply, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (select.name.toString == "$asInstanceOf") =>
          toTree(select.qualifier, returnValue)

        case _ =>
          NoMoreScriptTree()
      }
    }

    def toPrimitiveType(typeName: String) = {
      PRIMITIVE_TYPES.get(typeName).getOrElse(typeName)
    }

    def toDef(ddef: DefDef, packageName: Option[String]) = {
      val className = packageName match {
        case Some(packageName) => packageName + "." + ddef.symbol.owner.name.toString
        case _ => ddef.symbol.owner.name.toString
      }

      if (ddef.symbol.owner.tpe.typeSymbol.annotations.exists(_.toString == "com.github.suzuki0keiichi.nomorescript.annotation.global")) {
        if (ddef.name.toString == ddef.tpe.typeSymbol.enclClass.toString) {
          toTree(ddef.rhs, false)
        } else {
          NoMoreScriptDef(
            ddef.name.toString,
            ddef.vparamss.map(toParameterNames(_).toMap).toList,
            toTree(ddef.rhs, ddef.tpt.toString != "Unit"),
            None)
        }
      } else if (ddef.symbol.enclClass.isTrait) {
        NoMoreScriptTraitDef(
          ddef.name.toString,
          ddef.vparamss.map(toParameterNames(_).toMap).toList,
          toTree(ddef.rhs, ddef.tpt.toString != "Unit"),
          className)
      } else {
        NoMoreScriptDef(
          ddef.name.toString,
          ddef.vparamss.map(toParameterNames(_).toMap).toList,
          toTree(ddef.rhs, ddef.tpt.toString != "Unit"),
          Some(className))
      }
    }

    def toNew(nw: New, returnValue: Boolean) = {
      NoMoreScriptNew(toTree(nw.tpt, false, None))
    }

    def toThis(ths: This, returnValue: Boolean) = {
      if (isGlobal(ths)) {
        NoMoreScriptTree()
      } else {
        NoMoreScriptThis(returnValue)
      }
    }

    def isGlobal(tree: Tree): Boolean = {
      tree.tpe.typeSymbol.annotations.exists(_.toString == "com.github.suzuki0keiichi.nomorescript.annotation.global")
    }

    def toIdent(ident: Ident, returnValue: Boolean) = {
      if (isGlobal(ident)) {
        NoMoreScriptTree()
      } else {
        NoMoreScriptIdent(ident.name.toString, returnValue)
      }
    }

    def findClass(name: String): Option[ClassDef] = {
      findClass(name, localUnit.get._1.body)
    }

    def findClass(name: String, tree: Tree): Option[ClassDef] = {
      tree match {
        case cdef: ClassDef =>
          findConstructor(cdef) match {
            case Some(ddef) if (cdef.name.toString == name ||
              (ddef.tpt.toString.startsWith("anonymous class ") && ddef.tpt.toString.substring("anonymous class ".length) == name)) =>
              Some(cdef)

            case _ =>
              tree.children.map(findClass(name, _)).collectFirst {
                case Some(classDef) => classDef
              }
          }

        case _ =>
          tree.children.map(findClass(name, _)).collectFirst {
            case Some(classDef) => classDef
          }
      }
    }

    def toCase(caseDef: CaseDef, returnValue: Boolean) = {
      caseDef.pat match {
        case b: Bind =>
          b.body match {
            case t: Typed =>
              NoMoreScriptIf(
                NoMoreScriptInstanceOf(NoMoreScriptIdent("__match_target__", false), t.tpt.toString),
                NoMoreScriptTrees(List(
                  NoMoreScriptVal(b.name.toString, NoMoreScriptIdent("__match_target__", false), false),
                  toTree(caseDef.body, returnValue, None)), false),
                NoMoreScriptTree())
          }

        case i: Ident =>
          NoMoreScriptIf(NoMoreScriptTree(), toTree(caseDef.body, returnValue, None), NoMoreScriptTree())
      }
      //      NoMoreScriptTrees(
      //        List(toTree(caseDef.pat, false, None), toTree(caseDef.guard, false, None), toTree(caseDef.body, returnValue, None)), false)
    }

    def toTree(tree: Tree, returnValue: Boolean, namespace: Option[String] = None, memberNames: Map[String, String] = Map.empty[String, String]): NoMoreScriptTree = {
      tree match {
        case ths: This => toThis(ths, returnValue)
        case pdef: PackageDef => toPackage(pdef, namespace)
        case cdef: ClassDef if (!isAnonFun(cdef.name.toString)) => toClass(cdef, namespace)
        case vdef: ValDef => toVal(vdef, memberNames)
        case select: Select => toSelect(select, returnValue)
        case ident: Ident => toIdent(ident, returnValue)
        case nw: New => toNew(nw, returnValue)
        case fun: Function => NoMoreScriptJsFunction(toParameterNames(fun.vparams).toMap, toTree(fun.body, false))
        case sper: Super if (!BASE_CLASSES.contains(sper.symbol.superClass.name.toString)) =>
          NoMoreScriptSuper(toTree(sper.qual, returnValue, namespace, memberNames))

        case aplyImplicit: ApplyImplicitView if (aplyImplicit.fun.toString.startsWith("com.github.suzuki0keiichi.nomorescript.bridge.bridge.toJsFunction")) =>
          aplyImplicit.args(0) match {
            case fun: Function =>
              NoMoreScriptJsFunction(toParameterNames(fun.vparams).toMap, toTree(fun.body, false))

            case Block(_, Typed(Apply(Select(New(className), _), _), _)) if (className.toString.startsWith("anonymous class ")) =>
              findClass(className.toString.substring("anonymous class ".length)) match {
                case Some(cdef) => toAnonFun(cdef)

                case _ =>
                  addError(aplyImplicit.pos, "unknown error")
                  NoMoreScriptTree()
              }

            case _ =>
              NoMoreScriptTree()
          }

        case aply: Apply => toApply(aply, returnValue)

        case ddef: DefDef if (ddef.name.toString != "<init>" && ddef.name.toString != "$init$" && !ddef.mods.hasAccessorFlag) =>
          toDef(ddef, namespace)

        case block: Block =>
          val children = block.stats.map(toTree(_, false, namespace, memberNames)).toList :::
            List(toTree(block.expr, returnValue, namespace, memberNames))

          NoMoreScriptTrees(children, false)

        case Try(block, catches, finalizer) =>
          NoMoreScriptTry(
            toTree(block, returnValue, namespace, memberNames),
            NoMoreScriptCases(catches.map(toCase(_, returnValue))),
            toTree(finalizer, false, namespace, memberNames))

        case ifBlock: If =>
          NoMoreScriptIf(toTree(ifBlock.cond, false), toTree(ifBlock.thenp, returnValue), toTree(ifBlock.elsep, returnValue))

        case literal: Literal =>
          literal.value.value match {
            case s: String => NoMoreScriptLiteral("\"" + literal.value.value.toString + "\"", returnValue)
            case _ =>
              if (literal.value.value == null) {
                NoMoreScriptLiteral("null", returnValue)
              } else {
                NoMoreScriptLiteral(literal.value.value.toString, returnValue)
              }
          }

        case t: Throw =>
          NoMoreScriptThrow(toTree(t.expr, false, None))

        case m: Match =>
          // NoMoreScriptTrees(List(toTree(m.selector, false, None)) ::: m.cases.map(toCase(_, returnValue)), true)
          addError(m.pos, "pattern match is not supported")
          NoMoreScriptTree()

        case t: Tree =>
          NoMoreScriptTree()
      }
    }
  }

}
