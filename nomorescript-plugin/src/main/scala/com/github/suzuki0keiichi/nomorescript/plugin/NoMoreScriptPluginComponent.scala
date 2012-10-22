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

  def newPhase(prev: Phase) =
    new NoMoreScriptPhase(prev)

  class ScopedVariables(private val parent: ScopedVariables) {
    private val vars = scala.collection.mutable.Map[String, Symbol]()

    private def exists(name: String): Boolean = {
      if (vars.contains(name)) {
        true
      } else {
        false
      }
    }

    private def renamePut(origName: String, sym: Symbol, count: Int): String = {
      val newName = origName + "__scoped__" + count

      if (exists(newName)) {
        renamePut(origName, sym, count + 1)
      } else {
        put(newName, sym)
      }
    }

    def put(name: String, sym: Symbol): String = {
      if (exists(name)) {
        renamePut(name, sym, 1)
      } else {
        vars.put(name, sym)
        name
      }
    }

    def getName(sym: Symbol): String = {
      val currentResult = vars.collectFirst {
        case (name, varsSym) if (varsSym == sym) => name
      }

      currentResult match {
        case Some(name) => name
        case None => if (parent != null) { parent.getName(sym) } else { sym.name.toString() }
      }
    }
  }

  class NoMoreScriptPhase(prev: Phase) extends StdPhase(prev) {
    override def name: String = phaseName

    val BASE_CLASSES = List("java.lang.Object", "ScalaObject", "Product", "Serializable", "Object")
    val PRIMITIVE_TYPES = Map("Int" -> "number", "String" -> "string", "Any" -> "object")
    val localUnit = new ThreadLocal[(CompilationUnit, Boolean)]

    private def addError(pos: Position, message: String) {
      localUnit.get()._1.error(pos, message)
      localUnit.set((localUnit.get()._1, true))
    }

    def apply(unit: CompilationUnit) {
      localUnit.set((unit, false))

      val currentDir = new java.io.File(".")
      val scopedVars = new ScopedVariables(null)

      unit.body match {
        case pdef: PackageDef =>
          val js = toPackage(pdef, scopedVars).toJs(false)

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

    def toPackage(pdef: PackageDef, scopedVars: ScopedVariables) = {
      val name = pdef.name.toString.trim()

      getPackageName(pdef.symbol, null) match {
        case Some(name) =>
          NoMoreScriptNamespace(name, pdef.stats.map(toTree(_, scopedVars, false)))
        case None =>
          NoMoreScriptTrees(pdef.stats.map(toTree(_, scopedVars, false)), false)
      }
    }

    def getPackageName(symbol: Symbol, child: String): Option[String] = {
      if (symbol.isRoot || symbol.name.toString == "<empty>") {
        if (child == null) None else Some(child)
      } else {
        getPackageName(symbol.owner, if (child == null) symbol.name.toString else symbol.name.toString + "." + child)
      }
    }

    def getSuperTraitsWithParent(parent: Symbol): List[Symbol] = {
      val traits = parent.tpe.parents.collect {
        case t: UniqueTypeRef if (!BASE_CLASSES.contains(t.toString) && !t.typeSymbol.isTrait) =>
          getSuperTraitsWithParent(t.typeSymbol)

        case t: UniqueTypeRef if (!BASE_CLASSES.contains(t.toString) && t.typeSymbol.isTrait) =>
          getSuperTraitsWithParent(t.typeSymbol) ::: List(t.typeSymbol)
      }

      traits.flatten.distinct
    }

    def getAllSuperTraits(parent: Symbol, excludes: List[Symbol]): List[Symbol] = {
      val traits = parent.tpe.parents.collect {
        case t: UniqueTypeRef if (!BASE_CLASSES.contains(t.toString) && t.typeSymbol.isTrait && !excludes.contains(t.typeSymbol)) =>
          getSuperTraitsWithParent(t.typeSymbol) ::: List(t.typeSymbol)
      }

      traits.flatten.distinct
    }

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
          case t: Ident if (!BASE_CLASSES.contains(t.toString) && !t.tpe.typeSymbol.isTrait) => t
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

    def haveAnnotation(cdef: ClassDef, name: String): Boolean = {
      cdef.symbol.annotations.exists {
        a: AnnotationInfo => a.toString == name
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

    def toParameterNames(params: List[ValDef], scopedVar: ScopedVariables): Map[String, String] = {
      params.map {
        param =>
          scopedVar.put(param.name.toString, param.symbol) -> toPrimitiveType(param.symbol.tpe.toString)
      }.toMap
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
        case ddef: DefDef if (ddef.name.toString == "<init>" && !BASE_CLASSES.contains(ddef.symbol.enclClass.superClass.name.toString)) =>
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

    def isAnonFun(ddef: DefDef): Boolean = {
      isAnonFun(ddef.name.toString.trim)
    }

    def isAnonFun(name: String): Boolean = {
      name.startsWith("$anonfun$")
    }

    def toAnonFun(cdef: ClassDef, scopedVars: ScopedVariables) = {
      cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.tpt.toString == "Unit") => ddef
      } match {
        case Some(ddef) =>
          val newScopedVars = new ScopedVariables(scopedVars)
          NoMoreScriptJsFunction(None, toParameterNames(ddef.vparamss(0), newScopedVars).toMap, toTree(ddef.rhs, newScopedVars, ddef.tpt.toString != "Unit"))

        case _ =>
          NoMoreScriptEmpty()
      }
    }

    def toVal(vdef: ValDef, scopedVars: ScopedVariables, memberNames: Map[String, String]) = {
      val isMember = memberNames.contains(vdef.name.toString.trim())
      val name = if (isMember) {
        vdef.name.toString.trim()
      } else {
        scopedVars.put(vdef.name.toString.trim(), vdef.symbol)
      }

      if (vdef.rhs.toString.trim() == "<empty>") {
        if (isMember) {
          NoMoreScriptVal(name, NoMoreScriptIdent(name, false), isMember)
        } else {
          NoMoreScriptVal(name, NoMoreScriptIdent("null", false), false);
        }
      } else {
        NoMoreScriptVal(name, toTree(vdef.rhs, scopedVars, false), isMember)
      }
    }

    def toSelect(select: Select, scopedVars: ScopedVariables, returnValue: Boolean) = {
      if (select.name.toString == "package") {
        toTree(select.qualifier, scopedVars, returnValue)
      } else {
        select.symbol match {
          case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
            // TODO:toJsの方でやる
            if (m.referenced.enclClass.isTrait) {
              val name = getPackageName(m.referenced.enclClass, null).get + ".__impl__." + select.name.toString.substring("super$".length) + ".call"
              NoMoreScriptIdent(name, returnValue)
            } else {
              val name = getPackageName(m.referenced.enclClass, null).get + ".prototype." + select.name.toString.substring("super$".length) + ".call"
              NoMoreScriptIdent(name, returnValue)
            }

          case _ =>
            NoMoreScriptSelect(scopedVars.getName(select.symbol), toTree(select.qualifier, scopedVars, returnValue))
        }

      }
    }

    val setterMatcher = "([a-zA-Z0-9_]+)_\\$eq".r

    def toSetter(aply: Apply) = {
      aply.fun.asInstanceOf[Select].name.toString match {
        case setterMatcher(name) => Some(name)
        case a => None
      }
    }

    def toGetter(aply: Apply, scopedVars: ScopedVariables, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (aply.symbol.hasAccessorFlag) => Some(toSelect(select, scopedVars, returnValue))
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

    def toApply(aply: Apply, scopedVars: ScopedVariables, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (select.name.toString == "$isInstanceOf") =>
          NoMoreScriptInstanceOf(toTree(select.qualifier, scopedVars, returnValue), aply.args(0).toString)

        case typeApply: TypeApply =>
          toTypeApply(typeApply, scopedVars, returnValue)

        case select: Select =>
          toSetter(aply) match {
            case Some(setter) =>
              NoMoreScriptSetter(setter, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false))

            case None =>
              toGetter(aply, scopedVars, returnValue) match {
                case Some(getter) => getter
                case None =>
                  toOperator(aply) match {
                    case Some(operator) =>
                      NoMoreScriptOperator(operator, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false), returnValue)

                    case None =>
                      val paramForSuperMethod = select.symbol match {
                        case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
                          List(NoMoreScriptThis(false))
                        case _ => Nil
                      }

                      val params = paramForSuperMethod ++ aply.args.map(toTree(_, scopedVars, false))

                      NoMoreScriptApply(toSelect(select, scopedVars, false), params, returnValue, isArrayApply(aply))
                  }
              }
          }

        case _ =>
          val params = aply.args.map(toTree(_, scopedVars, false))

          NoMoreScriptApply(toTree(aply.fun, scopedVars, false), params, returnValue, isArrayApply(aply))
      }
    }

    def toTypeApply(aply: TypeApply, scopedVars: ScopedVariables, returnValue: Boolean) = {
      aply.fun match {
        case select: Select if (select.name.toString == "$asInstanceOf") =>
          toTree(select.qualifier, scopedVars, returnValue)

        case _ =>
          NoMoreScriptEmpty()
      }
    }

    def toPrimitiveType(typeName: String) = {
      if (typeName.indexOf("=>") != -1) {
        "Function"
      } else {
        PRIMITIVE_TYPES.get(typeName).getOrElse(typeName)
      }
    }

    def isGlobalClass(typeSymbol: Symbol): Boolean = {
      typeSymbol.annotations.exists(_.toString == "com.github.suzuki0keiichi.nomorescript.annotation.global")
    }

    def isGlobalClass(ddef: DefDef): Boolean = {
      isGlobalClass(ddef.symbol.owner.tpe.typeSymbol)
    }

    def isGlobalClass(tree: Ident): Boolean = {
      isGlobalClass(tree.tpe.typeSymbol)
    }

    def isGlobalClass(tree: This): Boolean = {
      isGlobalClass(tree.tpe.typeSymbol)
    }

    def toDef(ddef: DefDef, scopedVars: ScopedVariables) = {
      val className = getPackageName(ddef.symbol.owner.owner, null) match {
        case Some(packageName) => packageName + "." + ddef.symbol.owner.name.toString
        case _ => ddef.symbol.owner.name.toString
      }

      if (isGlobalClass(ddef)) {
        if (ddef.name.toString == ddef.tpe.typeSymbol.enclClass.toString) {
          // コンストラクタなので関数ではなくそのまま処理を記述
          toTree(ddef.rhs, scopedVars, false)
        } else {
          val newScope = new ScopedVariables(scopedVars)

          // global上の関数
          NoMoreScriptDef(
            ddef.name.toString,
            ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
            toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
            None)
        }
      } else if (ddef.symbol.enclClass.isTrait) {
        val newScope = new ScopedVariables(scopedVars)

        // traitの関数
        NoMoreScriptTraitDef(
          ddef.name.toString,
          ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
          toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
          className)
      } else {
        val newScope = new ScopedVariables(scopedVars)

        // 通常のクラス関数
        NoMoreScriptDef(
          ddef.name.toString,
          ddef.vparamss.map(toParameterNames(_, newScope).toMap).toList,
          toTree(ddef.rhs, newScope, ddef.tpt.toString != "Unit"),
          Some(className))
      }
    }

    def toNew(nw: New, scopedVars: ScopedVariables, returnValue: Boolean) = {
      NoMoreScriptNew(toTree(nw.tpt, scopedVars, false))
    }

    def toThis(ths: This, returnValue: Boolean) = {
      if (isGlobalClass(ths)) {
        NoMoreScriptEmpty()
      } else {
        NoMoreScriptThis(returnValue)
      }
    }

    def toIdent(ident: Ident, scopedVars: ScopedVariables, returnValue: Boolean) = {
      if (isGlobalClass(ident)) {
        NoMoreScriptEmpty()
      } else {
        val name = if (ident.symbol.isClass) {
          getPackageName(ident.symbol.owner, null) match {
            case Some(packageName) => packageName + "." + ident.name
            case None => ident.name.toString()
          }
        } else {
          scopedVars.getName(ident.symbol)
        }

        NoMoreScriptIdent(name, returnValue)
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

    def toTree(tree: Tree, scopedVars: ScopedVariables, returnValue: Boolean, memberNames: Map[String, String] = Map.empty[String, String]): NoMoreScriptTree = {
      tree match {
        case ths: This => toThis(ths, returnValue)
        case pdef: PackageDef => toPackage(pdef, scopedVars)
        case cdef: ClassDef if (!isAnonFun(cdef.name.toString)) => toClass(cdef, scopedVars)
        case vdef: ValDef => toVal(vdef, scopedVars, memberNames)
        case select: Select => toSelect(select, scopedVars, returnValue)
        case ident: Ident => toIdent(ident, scopedVars, returnValue)
        case nw: New => toNew(nw, scopedVars, returnValue)
        case fun: Function =>
          val newScope = new ScopedVariables(scopedVars)
          NoMoreScriptJsFunction(None, toParameterNames(fun.vparams, newScope).toMap, toTree(fun.body, newScope, !fun.tpe.resultType.toString.endsWith("=> Unit")))

        case sper: Super if (!BASE_CLASSES.contains(sper.symbol.superClass.name.toString)) =>
          val parent = sper.symbol.tpe.parents.collectFirst {
            case ref: UniqueTypeRef if (!ref.typeSymbol.isTrait) => ref.typeSymbol
          }

          NoMoreScriptIdent(getPackageName(parent.get, null).get, returnValue)

        case aplyImplicit: ApplyImplicitView if (aplyImplicit.fun.toString.startsWith("com.github.suzuki0keiichi.nomorescript.bridge.bridge.toJsFunction")) =>
          aplyImplicit.args(0) match {
            case fun: Function =>
              val newScope = new ScopedVariables(scopedVars)
              NoMoreScriptJsFunction(Some(fun.vparams.head.name.toString()), toParameterNames(fun.vparams.tail, newScope).toMap, toTree(fun.body, newScope, !fun.tpe.resultType.toString.endsWith("=> Unit")))

            case Block(_, Typed(Apply(Select(New(className), _), _), _)) if (className.toString.startsWith("anonymous class ")) =>
              findClass(className.toString.substring("anonymous class ".length)) match {
                case Some(cdef) => toAnonFun(cdef, scopedVars)

                case _ =>
                  addError(aplyImplicit.pos, "unknown error")
                  NoMoreScriptEmpty()
              }

            case _ =>
              NoMoreScriptEmpty()
          }

        case aply: Apply => toApply(aply, scopedVars, returnValue)

        case ddef: DefDef if (ddef.name.toString != "<init>" && ddef.name.toString != "$init$" && !ddef.mods.hasAccessorFlag) =>
          toDef(ddef, scopedVars)

        case block: Block =>
          val children = block.stats.map(toTree(_, scopedVars, false, memberNames)).toList :::
            List(toTree(block.expr, scopedVars, returnValue, memberNames))

          NoMoreScriptTrees(children, false)

        case Try(block, catches, finalizer) =>
          NoMoreScriptTry(
            toTree(block, scopedVars, returnValue, memberNames),
            NoMoreScriptCases(catches.map(toCase(_, scopedVars, returnValue))),
            toTree(finalizer, scopedVars, false, memberNames))

        case ifBlock: If =>
          NoMoreScriptIf(toTree(ifBlock.cond, scopedVars, false), toTree(ifBlock.thenp, scopedVars, returnValue), toTree(ifBlock.elsep, scopedVars, returnValue))

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
          NoMoreScriptThrow(toTree(t.expr, scopedVars, false))

        case m: Match =>
          // NoMoreScriptTrees(List(toTree(m.selector, false, None)) ::: m.cases.map(toCase(_, returnValue)), true)
          addError(m.pos, "pattern match is not supported")
          NoMoreScriptEmpty()

        case t: Tree =>
          NoMoreScriptEmpty()
      }
    }
  }

}
