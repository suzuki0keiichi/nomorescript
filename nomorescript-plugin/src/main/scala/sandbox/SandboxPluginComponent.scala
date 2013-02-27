package sandbox

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}
import java.io.IOException
import java.io.File
import scala.Some
import collection.mutable.ListBuffer
import com.github.suzuki0keiichi.nomorescript.trees.Util

/**
 * ポリシーとしてはあくまでJavaScript寄りの中間クラスに変換する
 * そこからjsdocやAMD、CommonJS、ClosureCompiler形式の出力を行う
 */
class SandboxPluginComponent(val global: Global, val plugin: SandboxPlugin) extends PluginComponent {

  import global._

  val runsAfter: List[String] = List("refchecks")
  val phaseName: String = "scala to javascript convert phase"

  def newPhase(prev: Phase) = new SandboxPhase(prev)

  implicit val options = ConvertOptions(ModuleType.CommonJS, plugin.srcDir, plugin.outputDir)

  class SandboxPhase(prev: Phase) extends StdPhase(prev) {

    override def name: String = phaseName

    implicit def nameToString(name: Name) = name.toString

    val localUnit = new ThreadLocal[(CompilationUnit, Boolean)]

    def isUserClass(cdef: ClassDef) = !cdef.name.startsWith("$anonfun$")

    def isJsFunction(aplyImplicit: ApplyImplicitView) = true

    def isUserMethod(ddef: DefDef) = true

    private def isGlobalClass(tree: Ident): Boolean = {
      false
      //      isGlobalClass(tree.tpe.typeSymbol)
    }

    def toParameterNames(params: List[ValDef], scopedVar: ScopedVariables): Map[String, String] = {
      params.map {
        param =>
          scopedVar.put(param.name.toString, param.symbol) -> PrimitiveTypes.toPrimitiveType(param.symbol.tpe.toString)
      }.toMap
    }

    def getPackageName(symbol: Symbol, childOpt: Option[String]): Option[String] = {
      if (symbol.isRoot || symbol.name.toString == "<empty>") {
        childOpt match {
          case Some(child) => Some(child)
          case None => None
        }
      } else {
        getPackageName(symbol.owner,
          childOpt match {
            case Some(child) => Some(symbol.name.toString + "." + child)
            case None => Some(symbol.name.toString)
          })
      }
    }

    def getPackageName(cdef: ClassDef): Option[String] = getPackageName(cdef.symbol.owner, None)

    def getFullName(cdef: ClassDef): String = {
      val name = cdef.name.toString

      getPackageName(cdef) match {
        case Some(packageName) => packageName + "." + name
        case None => name
      }
    }

    def hasError = localUnit.get._2

    def addError(pos: Position, message: String) {
      localUnit.get()._1.error(pos, message)
      localUnit.set((localUnit.get()._1, true))
    }

    object SystemClasses {
      val classNames = List("Any")

      def isSystemClass(name: Name) = classNames.contains(name.toString)
    }

    def apply(unit: CompilationUnit) {
      localUnit.set((unit, false))

      val scopedVars = new ScopedVariables(null)

      try {
        val convertedBody: List[String] = unit.body match {
          case t: Tree => convertTree(t, scopedVars)
        }

        if (!hasError) {
          val currentDir = new File(System.getProperties.getProperty("user.dir"))
          val writer = options.createWriter(currentDir, unit.source.file.file, global.settings)

          try {
            convertedBody.foreach(writer.println(_))
            writer.flush()
          } catch {
            case e: IOException => addError(NoPosition, e.getMessage)
          } finally {
            writer.close()
          }
        }
      } finally {
        localUnit.remove()
      }
    }

    def convertTree(tree: Tree, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier = NsPrefixModifier(), postfixModifier: NsPostfixModifier = NsPostfixModifier()): List[String] = {
      tree match {
        case pdef: PackageDef => convertPackageDef(pdef, scopedVars)

        case ths: This =>
          // globalアノテーションがついていた場合のみthisを付けない
          Nil

        case cdef: ClassDef if (isUserClass(cdef)) => convertClassDef(cdef)
        case vdef: ValDef => convertValDef(vdef, scopedVars)
        case ddef: DefDef if (isUserMethod(ddef)) => Nil
        case fun: Function => Nil

        case aplyImplicit: ApplyImplicitView =>
          if (isJsFunction(aplyImplicit)) {
            // 第一引数をthisとするfunction
            Nil
          } else {
            // 変換関数を挟む
            Nil
          }

        case aply: Apply => convertApply(aply)

        case block: Block =>
          // 上から代入とかreturnとか修飾子が来てたらそれを最後の評価式につけるだけ
          Nil

        case m: Match => Nil
        case ifBlock: If => Nil
        case Try(block, catches, finalizer) => Nil
        case select: Select => convertSelect(select, scopedVars, prefixModifier, postfixModifier)
        case ident: Ident => convertIdent(ident, scopedVars, prefixModifier)
        case nw: New => Nil
        case literal: Literal => convertLiteral(literal, prefixModifier)
        case sper: Super if (!SystemClasses.isSystemClass(sper.symbol.superClass.name.toString)) =>
          // 親クラスを直接指定
          Nil

        case t: Throw => Nil

        case t: Tree =>
          addError(t.pos, "unknown tree type " + t)
          Nil

      }
    }

    def convertDoWhile(): List[String] = {
      Nil
    }

    def convertIfElse(): List[String] = {
      Nil
    }

    def convertFor(): List[String] = {
      Nil
    }

    def convertTryCatchFinally(): List[String] = {
      Nil
    }

    def convertBlock(): List[String] = {
      Nil
    }

    def convertCollectionDef(): List[String] = {
      Nil
    }

    /**
     * hasModuleの場合だとglobal的な扱い
     * isTraitの時だとtraitとしての変換
     * classの時だとclassとしての変換だが、traitは総なめをして自classだけで実装するもののみ実装する
     * 依存クラスもリストアップし、AMDに備える
     */
    def convertClassDef(cdef: ClassDef): List[String] = {
      convertConstructorDef(cdef)
    }

    def convertObjectDef(): List[String] = {
      Nil
    }

    def convertTraitDef(): List[String] = {
      Nil
    }

    def convertMethodDef(): List[String] = {
      Nil
    }

    def convertSelect(select: Select, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier, postfixModifier: NsPostfixModifier): List[String] = {
      if (select.name.toString == "package") {
        convertTree(select.qualifier, scopedVars, prefixModifier, postfixModifier)
      } else {
        select.symbol match {
          // 親クラスの関数を指定している場合は参照の場所を変える必要がある
          // traitの場合はprototypeに居ないので更に呼び出し方を変える
          // TODO:あとでコメント解除
          //          case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
          //            // TODO:toJsの方でやる
          //            if (m.referenced.enclClass.isTrait) {
          //              val name = getPackageName(m.referenced.enclClass, None).get + ".__impl__." + select.name.toString.substring("super$".length) + ".call"
          //              NsIdent(name, prefixModifier)
          //            } else {
          //              val name = getPackageName(m.referenced.enclClass, None).get + ".prototype." + select.name.toString.substring("super$".length) + ".call"
          //              NsIdent(name, prefixModifier)
          //            }

          case _ =>
            val childJs = convertTree(select.qualifier, scopedVars)
            val name = scopedVars.getName(select.symbol)

            val js =
              if (name == "<init>") {
                postfixModifier.modify(childJs)
              } else if (childJs == Nil) {
                List(postfixModifier.modify(name))
              } else {
                postfixModifier.modify(Util.addLast(childJs, "." + name))
              }

            prefixModifier.modify(js)
        }

      }
    }

    def convertIdent(ident: Ident, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier): List[String] = {
      if (isGlobalClass(ident)) {
        Nil
      } else {
        val name = if (ident.symbol.isClass) {
          getPackageName(ident.symbol.owner, null) match {
            case Some(packageName) => packageName + "." + ident.name
            case None => ident.name.toString()
          }
        } else {
          scopedVars.getName(ident.symbol)
        }

        List(prefixModifier.modify(name))
      }
    }

    /**
     * パッケージ宣言とその下に付くクラスひと通り
     */
    def convertPackageDef(pdef: PackageDef, scopedVars: ScopedVariables): List[String] = {
      getPackageName(pdef.symbol, None) match {
        case Some(name) =>
          val js = ListBuffer[String]()
          val splittedNames = name.split("\\.")

          js.append("if (!" + splittedNames(0) + ") " + splittedNames(0) + " = new ns.Namespace(\"" + splittedNames(0) + "\");")
          if (splittedNames.size > 1) {
            js.append(splittedNames(0) + ".add([" + splittedNames.tail.map("\"" + _ + "\"").mkString(", ") + "]);")
          }

          js.append("")
          js.toList ::: pdef.stats.flatMap(convertTree(_, scopedVars))

        case None =>
          pdef.stats.flatMap(convertTree(_, scopedVars))
      }
    }

    def convertImplicit(): List[String] = {
      Nil
    }

    /**
     * メンバかどうかでthisつけるか変わる、代入元がemptyかどうかでnullか変わる
     */
    def convertValDef(vdef: ValDef, scopedVars: ScopedVariables): List[String] = {
      val isMember = vdef.symbol.owner.isInstanceOf[ClassSymbol]

      val name = if (isMember) {
        vdef.name.toString.trim()
      } else {
        scopedVars.put(vdef.name.toString.trim(), vdef.symbol)
      }

      if (vdef.rhs.toString.trim() == "<empty>") {
        if (isMember) {
          List("this." + name + " = " + name + ";")
        } else {
          List("var " + name + " = null;")
        }
      } else {
        List((if (isMember) "this." else "var ") + name + " = " + convertTree(vdef.rhs, scopedVars).mkString(" ") + ";")
      }
    }

    def convertApply(aply: Apply): List[String] = {
      // TODO:あとでコメント解除
      //      aply.fun match {
      //        case select: Select if (select.name.toString == "$isInstanceOf") =>
      //          NoMoreScriptInstanceOf(toTree(select.qualifier, scopedVars, returnValue), aply.args(0).toString)
      //
      //        case typeApply: TypeApply =>
      //          toTypeApply(typeApply, scopedVars, returnValue)
      //
      //        case select: Select =>
      //          toSetter(aply) match {
      //            case Some(setter) =>
      //              NoMoreScriptSetter(setter, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false))
      //
      //            case None =>
      //              toGetter(aply, scopedVars, returnValue) match {
      //                case Some(getter) => getter
      //                case None =>
      //                  toOperator(aply) match {
      //                    case Some(operator) =>
      //                      NoMoreScriptOperator(operator, toTree(aply.fun.asInstanceOf[Select].qualifier, scopedVars, false), toTree(aply.args(0), scopedVars, false), returnValue)
      //
      //                    case None =>
      //                      val paramForSuperMethod = select.symbol match {
      //                        case m: MethodSymbol if (select.name.toString.indexOf("super$") != -1) =>
      //                          List(NoMoreScriptThis(false))
      //                        case _ => Nil
      //                      }
      //
      //                      val params = paramForSuperMethod ++ aply.args.map(toTree(_, scopedVars, false))
      //
      //                      NoMoreScriptApply(toSelect(select, scopedVars, false), params, returnValue, isArrayApply(aply))
      //                  }
      //              }
      //          }
      //
      //        case _ =>
      //          val params = aply.args.map(toTree(_, scopedVars, false))
      //
      //          NoMoreScriptApply(toTree(aply.fun, scopedVars, false), params, returnValue, isArrayApply(aply))
      //      }
      Nil
    }

    def convertOperator(): List[String] = {
      Nil
    }

    def convertConstructorDef(cdef: ClassDef): List[String] = {
      val scopedVars = new ScopedVariables(null)

      val params: Map[String, String] = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString.trim() == "<init>") => toParameterNames(ddef.vparamss.head, scopedVars)
      }.getOrElse(Map[String, String]())

      val fields: Map[String, String] = cdef.impl.body.collect {
        case vdef: ValDef =>
          vdef.name.toString.trim() -> PrimitiveTypes.toPrimitiveType(vdef.tpt.toString)
      }.toMap

      // TODO:あとでコメント解除
      //      val callSuperClass = cdef.impl.body.collectFirst {
      //        case ddef: DefDef if (ddef.name.toString == "<init>" && !BaseClasses.isBaseClass(ddef.symbol.enclClass.superClass.name.toString)) =>
      //          ddef.rhs match {
      //            case b: Block => b.stats.collect {
      //              case aply: Apply =>
      //                convertTree(aply, newScopedVars, false) match {
      //                  case aply: NsCall => aply.toSuperConstructorCall()
      //                  case _ => NsEmpty()
      //                }
      //            }
      //
      //            case _ => Nil
      //          }
      //      }.getOrElse(Nil)e

      // TODO:あとでコメント解除
      //      val callSuperTraits = superTraitNames.map {
      //        name =>
      //          NoMoreScriptApply(NoMoreScriptSelect("call", NoMoreScriptIdent(name, false)), List(NoMoreScriptIdent("this", false)), false, false)
      //      }

      val bodies = cdef.impl.body.filter {
        case ddef: DefDef => false
        case tree: Tree => true
      }.flatMap(convertTree(_, scopedVars))

      // TODO:あとでコメント解除
      // NoMoreScriptConstructor(name, params, memberNames, callSuperClass ++ callSuperTraits ++ bodies)
      List("/**", " * @constructor") :::
        fields.map(field => " * @property {" + field._2 + "} " + field._1).toList :::
        params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
        List(" */", getFullName(cdef) + " = function (" + params.map(param => param._1).mkString(", ") + ") {") :::
        bodies.map(options.indent + _) :::
        List("};", "")
    }

    def convertPatternMatch(): List[String] = {
      Nil
    }

    def convertLiteral(literal: Literal, prefixModifier: NsPrefixModifier): List[String] = {
      literal.value.value match {
        case s: String =>
          List(prefixModifier.modify("\"" + literal.value.value.toString + "\""))

        case _ =>
          if (literal.value.value == null) {
            List(prefixModifier.modify("null"))
          } else {
            List(prefixModifier.modify(literal.value.value.toString))
          }
      }
    }
  }

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
        case None => if (parent != null) {
          parent.getName(sym)
        } else {
          sym.name.toString()
        }
      }
    }
  }

}