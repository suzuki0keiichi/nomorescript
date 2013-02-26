package sandbox

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}
import java.io.IOException
import java.io.File
import scala.Some

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
        val convertedBody: NsEmpty = unit.body match {
          case t: Tree => convertTree(t, scopedVars)
        }

        if (!hasError) {
          val currentDir = new File(System.getProperties.getProperty("user.dir"))
          val writer = options.createWriter(currentDir, unit.source.file.file, global.settings)

          try {
            convertedBody.toJs.foreach(writer.println(_))
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

    def convertTree(tree: Tree, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier = NsPrefixModifier(), postfixModifier: NsPostfixModifier = NsPostfixModifier()): NsEmpty = {
      tree match {
        case pdef: PackageDef => convertPackageDef(pdef, scopedVars)

        case ths: This =>
          // globalアノテーションがついていた場合のみthisを付けない
          NsEmpty()

        case cdef: ClassDef if (isUserClass(cdef)) => convertClassDef(cdef)
        case vdef: ValDef => convertValDef(vdef, scopedVars)
        case ddef: DefDef if (isUserMethod(ddef)) => NsEmpty()
        case fun: Function => NsEmpty()

        case aplyImplicit: ApplyImplicitView =>
          if (isJsFunction(aplyImplicit)) {
            // 第一引数をthisとするfunction
            NsEmpty()
          } else {
            // 変換関数を挟む
            NsEmpty()
          }

        case aply: Apply => convertApply(aply)

        case block: Block =>
          // 上から代入とかreturnとか修飾子が来てたらそれを最後の評価式につけるだけ
          NsEmpty()

        case m: Match => NsEmpty()
        case ifBlock: If => NsEmpty()
        case Try(block, catches, finalizer) => NsEmpty()
        case select: Select => convertSelect(select, scopedVars, prefixModifier, postfixModifier)
        case ident: Ident => convertIdent(ident, scopedVars, prefixModifier)
        case nw: New => NsEmpty()
        case literal: Literal => convertLiteral(literal, prefixModifier)
        case sper: Super if (!SystemClasses.isSystemClass(sper.symbol.superClass.name.toString)) =>
          // 親クラスを直接指定
          NsEmpty()

        case t: Throw => NsEmpty()

        case t: Tree =>
          addError(t.pos, "unknown tree type " + t)
          NsEmpty()

      }
    }

    def convertDoWhile(): NsEmpty = {
      NsEmpty()
    }

    def convertIfElse(): NsEmpty = {
      NsEmpty()
    }

    def convertFor(): NsEmpty = {
      NsEmpty()
    }

    def convertTryCatchFinally(): NsEmpty = {
      NsEmpty()
    }

    def convertBlock(): NsEmpty = {
      NsEmpty()
    }

    def convertCollectionDef(): NsEmpty = {
      NsEmpty()
    }

    /**
     * hasModuleの場合だとglobal的な扱い
     * isTraitの時だとtraitとしての変換
     * classの時だとclassとしての変換だが、traitは総なめをして自classだけで実装するもののみ実装する
     * 依存クラスもリストアップし、AMDに備える
     */
    def convertClassDef(cdef: ClassDef): NsEmpty = {
      NsClassDef(getFullName(cdef), convertConstructorDef(cdef))
    }

    def convertObjectDef(): NsEmpty = {
      NsEmpty()
    }

    def convertTraitDef(): NsEmpty = {
      NsEmpty()
    }

    def convertMethodDef(): NsEmpty = {
      NsEmpty()
    }

    def convertSelect(select: Select, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier, postfixModifier: NsPostfixModifier): NsEmpty = {
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
            NsSelect(scopedVars.getName(select.symbol), convertTree(select.qualifier, scopedVars), prefixModifier, postfixModifier)
        }

      }
    }

    def convertIdent(ident: Ident, scopedVars: ScopedVariables, prefixModifier: NsPrefixModifier): NsEmpty = {
      if (isGlobalClass(ident)) {
        NsEmpty()
      } else {
        val name = if (ident.symbol.isClass) {
          getPackageName(ident.symbol.owner, null) match {
            case Some(packageName) => packageName + "." + ident.name
            case None => ident.name.toString()
          }
        } else {
          scopedVars.getName(ident.symbol)
        }

        NsIdent(name, prefixModifier)
      }
    }

    /**
     * パッケージ宣言とその下に付くクラスひと通り
     */
    def convertPackageDef(pdef: PackageDef, scopedVars: ScopedVariables): NsEmpty = {
      getPackageName(pdef.symbol, None) match {
        case Some(name) => NsList(NsNamespaceDef(name) :: pdef.stats.map(convertTree(_, scopedVars)), false)
        case None => NsList(pdef.stats.map(convertTree(_, scopedVars)), false)
      }
    }

    def convertImplicit(): NsEmpty = {
      NsEmpty()
    }

    /**
     * メンバかどうかでthisつけるか変わる、代入元がemptyかどうかでnullか変わる
     */
    def convertValDef(vdef: ValDef, scopedVars: ScopedVariables): NsEmpty = {
      val isMember = vdef.symbol.owner.isInstanceOf[ClassSymbol]

      val name = if (isMember) {
        vdef.name.toString.trim()
      } else {
        scopedVars.put(vdef.name.toString.trim(), vdef.symbol)
      }

      if (vdef.rhs.toString.trim() == "<empty>") {
        if (isMember) {
          NsVariable(name, NsIdent(name), true)
        } else {
          NsVariable(name, NsIdent("null"), false)
        }
      } else {
        NsVariable(name, convertTree(vdef.rhs, scopedVars), isMember)
      }
    }

    def convertApply(aply: Apply): NsEmpty = {
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
      NsEmpty()
    }

    def convertOperator(): NsEmpty = {
      NsEmpty()
    }

    def convertConstructorDef(cdef: ClassDef): NsConstructorDef = {
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
      }.map(convertTree(_, scopedVars))

      // TODO:あとでコメント解除
      // NoMoreScriptConstructor(name, params, memberNames, callSuperClass ++ callSuperTraits ++ bodies)
      NsConstructorDef(getFullName(cdef), fields, params, bodies)
    }

    def convertPatternMatch(): NsEmpty = {
      NsEmpty()
    }

    def convertLiteral(literal: Literal, prefixModifier: NsPrefixModifier): NsEmpty = {
      literal.value.value match {
        case s: String =>
          NsLiteral("\"" + literal.value.value.toString + "\"", prefixModifier)

        case _ =>
          if (literal.value.value == null) {
            NsIdent("null", prefixModifier)
          } else {
            NsLiteral(literal.value.value.toString, prefixModifier)
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