package com.github.suzuki0keiichi.nomorescript.plugin

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

    val BASE_CLASSES = List("java.lang.Object", "ScalaObject")
    val localUnit = new ThreadLocal[(CompilationUnit, Boolean)]

    private def addError(pos: Position, message: String) = {
      {
        val out2 = new java.io.FileOutputStream("/tmp/nomorescript.log")

        out2.write((message + " " + pos).getBytes())
        out2.close()
      }
      localUnit.get()._1.error(pos, message)
      localUnit.set((localUnit.get()._1, true))
    }

    def apply(unit: CompilationUnit): Unit = {
      localUnit.set((unit, false))

      val currentDir = new java.io.File(".")

      unit.body match {
        case pdef: PackageDef =>
          val js = toPackage(pdef, None).toJs(false)

          if (!localUnit.get._2) {
            val file = unit.source.file.file
            val f = file.getParent()
            val g = parent.srcRootDir
            val relativePath =
              if (file.getParent().startsWith(parent.srcRootDir)) {
                file.getParent().substring(parent.srcRootDir.length())
              } else {
                file.getParent()
              }

            val outputDirRoot = {
              val file = new java.io.File(parent.outputDir)

              if (file.isAbsolute()) {
                file.getAbsolutePath()
              } else {
                currentDir.getAbsolutePath() + "/" + file.getPath()
              }
            }

            val outputDir = new java.io.File(outputDirRoot + "/" + relativePath)

            val a = currentDir.getAbsolutePath()
            val b = parent.outputDir
            val c = file.getParent()

            outputDir.mkdirs()

            {
              val out2 = new java.io.FileOutputStream("/tmp/nomorescript.log")

              out2.write(("yobareta yo2").getBytes())
              out2.close()
            }

            try {
              val writer = new java.io.PrintWriter(outputDir.getPath() + "/" + file.getName().replaceAll(".scala", "") + ".js", "UTF-8")
              try {
                js.foreach(writer.println(_))
                writer.flush()
              } finally {
                writer.close()
              }
            } catch {
              case e: Exception =>
                {
                  val out2 = new java.io.FileOutputStream("/tmp/nomorescript.log")

                  out2.write((e.toString()).getBytes())
                  out2.close()
                }
                throw e
            }
          }

        case t: Tree =>
          unit.error(t.pos, "not supported " + t.getClass())
      }
    }

    def toPackage(pdef: PackageDef, parentPackageName: Option[String]) = {
      val name = pdef.name.toString().trim()
      if (NoMoreScriptNamespace.isEmpty(name)) {
        NoMoreScriptTrees(pdef.stats.map(toTree(_, false, None)), false)
      } else {
        val namespace = parentPackageName.map(_ + ".").getOrElse("") + pdef.pid.toString()

        NoMoreScriptNamespace(namespace, pdef.stats.map(toTree(_, false, None, Some(namespace), Nil)))
      }
    }

    def toClass(cdef: ClassDef, globalClass: Option[String], namespace: Option[String]): NoMoreScriptTree = {
      if (cdef.mods.hasModuleFlag) {
        toModule(cdef)
      } else {
        if (!isSupportedConstructor(cdef)) {
          addError(cdef.pos, "multi constructor is not supported")

          return NoMoreScriptTree()
        }

        if (cdef.symbol.annotations.exists(a => a.toString() == "com.github.suzuki0keiichi.nomorescript.annotation.mock")) {
          return NoMoreScriptTree()
        }

        val name = cdef.name.toString().trim()

        NoMoreScriptClass(name, namespace, toConstructor(cdef, globalClass, namespace), cdef.impl.body.collect {
          case ddef: DefDef => toTree(ddef, false, globalClass, namespace, Nil, Some(name))
        })
      }
    }

    def toModule(cdef: ClassDef) = {
      val global = cdef.symbol.annotations.exists(a => a.toString() == "com.github.suzuki0keiichi.nomorescript.annotation.global")

      if (global) {
        NoMoreScriptTrees(cdef.impl.body.map(toTree(_, false, Some(cdef.name.toString()))), true)
      } else {
        addError(cdef.pos, "singleton class is not supported")
        NoMoreScriptTree()
      }
    }

    def isConstructor(ddef: DefDef) = {
      ddef.name.toString().trim() == "<init>" && !ddef.mods.hasAccessorFlag
    }

    def isSupportedConstructor(cdef: ClassDef) = {
      val constructorsCount = cdef.impl.body.count {
        //        case ddef: DefDef if (ddef.name.toString().trim() != "$init$" && !ddef.mods.hasAccessorFlag && ddef.name.toString().trim() == "<init>") => true
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

    def toConstructor(cdef: ClassDef, globalClass: Option[String], namespace: Option[String]) = {
      /*
      findConstructor(cdef) match {
        case Some(ddef) => NoMoreScriptConstructor(cdef.toString(), ddef.vparamss.flatMap(_.map(_.name.toString().trim())), List(toTree(ddef.rhs, false, None)))
        case _ => NoMoreScriptConstructor(cdef.toString().trim(), Nil, Nil)
      }
      */
      val params: List[String] = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString().trim() == "<init>") => ddef.vparamss.flatMap(_.map(_.name.toString().trim()))
      }.getOrElse(Nil)

      val memberNames = cdef.impl.body.collect {
        case ddef: DefDef if (ddef.mods.hasAccessorFlag) => ddef.name.toString().trim()
      }

      val bodies = cdef.impl.body.filter {
        case ddef: DefDef => false
        case tree: Tree => true
      }.map(toTree(_, false, globalClass, namespace, memberNames))

      val name = cdef.name.toString().trim()

      NoMoreScriptConstructor(name, params, bodies)
    }

    def isAnonFun(ddef: DefDef): Boolean = {
      isAnonFun(ddef.name.toString().trim)
    }

    def isAnonFun(name: String): Boolean = {
      name.startsWith("$anonfun$")
    }

    def toAnonFun(cdef: ClassDef, globalClass: Option[String]) = {
      cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.tpt.toString() == "Unit") => ddef
      } match {
        case Some(ddef) => NoMoreScriptJsFunction(ddef.vparamss(0).map(_.name.toString()), toTree(ddef.rhs, ddef.tpt.toString() != "Unit", globalClass))
        case _ => NoMoreScriptTree()
      }
    }

    def toVal(vdef: ValDef, memberNames: List[String], globalClass: Option[String]) = {
      val name = vdef.name.toString().trim()
      val isMember = memberNames.contains(name)

      if (vdef.rhs.toString().trim() == "<empty>") {
        if (isMember) {
          NoMoreScriptVal(name, NoMoreScriptIdent(name, false), isMember)
        } else {
          NoMoreScriptTree()
        }
      } else {
        NoMoreScriptVal(name, toTree(vdef.rhs, false, globalClass), isMember)
      }
    }

    def toSelect(select: Select, returnValue: Boolean, globalClass: Option[String]) = {
      if (select.name.toString() == "package") {
        toTree(select.qualifier, returnValue, globalClass)
      } else {
        NoMoreScriptSelect(select.name.toString(), toTree(select.qualifier, returnValue, globalClass: Option[String]))
      }
    }

    val setterMatcher = "([a-zA-Z0-9_]+)_\\$eq".r

    def toSetter(aply: Apply) = {
      aply.fun.asInstanceOf[Select].name.toString() match {
        case setterMatcher(name) => Some(name)
        case a => None
      }
    }

    def toGetter(aply: Apply, returnValue: Boolean, globalClass: Option[String]) = {
      aply.fun match {
        case select: Select if (aply.symbol.hasAccessorFlag) => Some(toSelect(select, returnValue, globalClass))
        case _ => None
      }
    }

    def toOperator(aply: Apply) = {
      aply.fun.asInstanceOf[Select].name.toString() match {
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
        case Select(qualifier, name) if (name.toString() == "apply" && COLLECTION_NAMES.exists(name => qualifier.tpe.toString().indexOf(name + "[") != -1)) =>
          true

        case Select(Select(Ident(packageName), companionName), name) if (name.toString() == "apply" && COLLECTION_NAMES.exists(_ == companionName.toString()) && packageName.toString() == "scala") =>
          true

        case _ =>
          false
      }
    }

    def toApply(aply: Apply, returnValue: Boolean, globalClass: Option[String]) = {
      aply.fun match {
        case select: Select if (select.name.toString() == "$isInstanceOf") =>
          NoMoreScriptInstanceOf(toTree(select.qualifier, returnValue, globalClass), aply.args(0).toString())

        case typeApply: TypeApply =>
          toTypeApply(typeApply, returnValue, globalClass)

        case select: Select =>
          toSetter(aply) match {
            case Some(setter) =>
              NoMoreScriptSetter(setter, toTree(aply.fun.asInstanceOf[Select].qualifier, false, None, globalClass), toTree(aply.args(0), false, globalClass))

            case None =>
              toGetter(aply, returnValue, globalClass) match {
                case Some(getter) => getter
                case None =>
                  toOperator(aply) match {
                    case Some(operator) =>
                      NoMoreScriptOperator(operator, toTree(aply.fun.asInstanceOf[Select].qualifier, false, None, globalClass), toTree(aply.args(0), false, globalClass), returnValue)

                    case None =>
                      val params = aply.args.map(toTree(_, false, None, globalClass))

                      NoMoreScriptApply(toSelect(select, false, globalClass), params, returnValue, isArrayApply(aply))
                  }
              }
          }

        case _ =>
          val params = aply.args.map(toTree(_, false, None, globalClass))

          NoMoreScriptApply(toTree(aply.fun, false, None, globalClass), params, returnValue, isArrayApply(aply))
      }
    }

    def toTypeApply(aply: TypeApply, returnValue: Boolean, globalClass: Option[String]) = {
      aply.fun match {
        case select: Select if (select.name.toString() == "$asInstanceOf") =>
          toTree(select.qualifier, returnValue, globalClass)

        case _ =>
          NoMoreScriptTree()
      }
    }

    def toDef(ddef: DefDef, classNameForDef: Option[String], globalClass: Option[String]) = {
      globalClass match {
        case Some(globalClassName) if (ddef.name.toString().trim() == globalClassName) =>
          toTree(ddef.rhs, false, globalClass)

        case Some(globalClassName) if (classNameForDef == globalClassName) =>
          NoMoreScriptDef(ddef.name.toString(), ddef.vparamss.flatMap(_.map(_.name.toString())), toTree(ddef.rhs, ddef.tpt.toString() != "Unit", globalClass), None)

        case _ =>
          NoMoreScriptDef(ddef.name.toString(), ddef.vparamss.flatMap(_.map(_.name.toString())), toTree(ddef.rhs, ddef.tpt.toString() != "Unit", globalClass), classNameForDef)
      }
    }

    def toNew(nw: New, returnValue: Boolean) = {
      NoMoreScriptNew(toTree(nw.tpt, false, None))
    }

    def toThis(ths: This, returnValue: Boolean, globalClass: Option[String] = None) = {
      globalClass match {
        case Some(globalClassName) if (ths.qual.toString() == globalClassName) => NoMoreScriptTree()
        case _ =>
          NoMoreScriptThis(returnValue)
      }
    }

    def toIdent(ident: Ident, returnValue: Boolean) = NoMoreScriptIdent(ident.name.toString(), returnValue)

    def findClass(name: String): Option[ClassDef] = {
      findClass(name, localUnit.get._1.body)
    }

    def findClass(name: String, tree: Tree): Option[ClassDef] = {
      tree match {
        case cdef: ClassDef =>
          findConstructor(cdef) match {
            case Some(ddef) if (cdef.name.toString() == name ||
              (ddef.tpt.toString().startsWith("anonymous class ") && ddef.tpt.toString().substring("anonymous class ".length) == name)) =>
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
                NoMoreScriptInstanceOf(NoMoreScriptIdent("_", false), t.tpt.toString()),
                NoMoreScriptTrees(List(
                  NoMoreScriptVal(b.name.toString(), NoMoreScriptIdent("_", false), false),
                  toTree(caseDef.body, returnValue, None)), false),
                NoMoreScriptTree())

            case _ =>
              println("wakaran1 " + b.body + " " + b.body.getClass())
              NoMoreScriptTree()
          }

        case i: Ident =>
          NoMoreScriptBlock(toTree(caseDef.body, returnValue, None), true)
        case _ =>
          println("wakaran2 " + caseDef.pat + " " + caseDef.pat.getClass())
          NoMoreScriptTree()
      }
      //      NoMoreScriptTrees(
      //        List(toTree(caseDef.pat, false, None), toTree(caseDef.guard, false, None), toTree(caseDef.body, returnValue, None)), false)
    }

    def toTree(tree: Tree, returnValue: Boolean, globalClass: Option[String], namespace: Option[String] = None, memberNames: List[String] = Nil, classNameForDef: Option[String] = None): NoMoreScriptTree = {
      tree match {
        case ths: This => toThis(ths, returnValue, globalClass)
        case pdef: PackageDef => toPackage(pdef, namespace)
        case cdef: ClassDef if (!isAnonFun(cdef.name.toString())) => toClass(cdef, globalClass, namespace)
        case vdef: ValDef => toVal(vdef, memberNames, globalClass)
        case select: Select => toSelect(select, returnValue, globalClass)
        case ident: Ident => toIdent(ident, returnValue)
        case nw: New => toNew(nw, returnValue)

        case aplyImplicit: ApplyImplicitView if (aplyImplicit.fun.toString().startsWith("com.github.suzuki0keiichi.nomorescript.bridge.bridge.toJsFunction")) =>
          aplyImplicit.args(0) match {
            case fun: Function =>
              NoMoreScriptJsFunction(fun.vparams.map(_.name.toString()), toTree(fun.body, false, globalClass))

            case Block(_, Typed(Apply(Select(New(className), _), _), _)) if (className.toString.startsWith("anonymous class ")) =>
              findClass(className.toString.substring("anonymous class ".length)) match {
                case Some(cdef) => toAnonFun(cdef, globalClass)

                case _ =>
                  addError(aplyImplicit.pos, "unknown error")
                  NoMoreScriptTree()
              }

            case _ =>
              NoMoreScriptTree()
          }

        case aply: Apply => toApply(aply, returnValue, globalClass)

        case ddef: DefDef if (ddef.name.toString() != "<init>" && ddef.name.toString() != "$init$" && !ddef.mods.hasAccessorFlag) =>
          toDef(ddef, classNameForDef, globalClass)

        case block: Block =>
          val children = block.stats ::: List(block.expr)
          val a = for (i <- 0 until children.size; tree = toTree(children(i), if (i == children.size - 1) returnValue else false, globalClass, namespace, memberNames)) yield tree

          NoMoreScriptTrees(a.toList, false)

        case Try(block, catches, finalizer) =>
          NoMoreScriptTry(
            toTree(block, returnValue, globalClass, namespace, memberNames, classNameForDef),
            catches.map(toCase(_, returnValue)),
            toTree(finalizer, false, globalClass, namespace, memberNames, classNameForDef))

        case ifBlock: If =>
          NoMoreScriptIf(toTree(ifBlock.cond, false, globalClass), toTree(ifBlock.thenp, returnValue, globalClass), toTree(ifBlock.elsep, returnValue, globalClass))

        case literal: Literal =>
          literal.value.value match {
            case s: String => NoMoreScriptLiteral("\"" + literal.value.value.toString() + "\"", returnValue)
            case _ =>
              if (literal.value.value == null) {
                NoMoreScriptLiteral("null", returnValue)
              } else {
                NoMoreScriptLiteral(literal.value.value.toString(), returnValue)
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