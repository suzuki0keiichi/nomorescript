package sandbox

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}
import java.io.IOException
import java.io.File
import scala.Some
import sandbox.ScopedVariables
import com.github.suzuki0keiichi.nomorescript.converter.{BaseClasses, PrimitiveTypes}

/**
 * ポリシーとしてはあくまでJavaScript寄りの中間クラスに変換する
 * そこからjsdocやAMD、CommonJS、ClosureCompiler形式の出力を行う
 */
class SandboxPluginComponent(val global: Global, val plugin: SandboxPlugin) extends PluginComponent with PackageHelper {

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

    def toParameterNames(params: List[ValDef], scopedVar: ScopedVariables): Map[String, String] = {
      params.map {
        param =>
          scopedVar.put(param.name.toString, param.symbol) -> PrimitiveTypes.toPrimitiveType(param.symbol.tpe.toString)
      }.toMap
    }

    def getPackageName(symbol: Symbol, child: Option[String]): Option[String] = {
      if (symbol.isRoot || symbol.name.toString == "<empty>") {
        child match {
          case Some(child) => Some(child)
          case None => None
        }
      } else {
        getPackageName(symbol.owner,
          child match {
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
          case t: Tree => convertTree(t, scopedVars, false)
        }

        if (!hasError) {
          val currentDir = new File(System.getProperties().getProperty("user.dir"))
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

    def convertTree(tree: Tree, scopedVars: ScopedVariables, terminate: Boolean): NsEmpty = {
      tree match {
        case pdef: PackageDef => convertPackageDef(pdef, scopedVars)

        case ths: This =>
          // globalアノテーションがついていた場合のみthisを付けない
          NsEmpty()

        case cdef: ClassDef if (isUserClass(cdef)) =>
          // hasModuleの場合だとglobal的な扱い
          // isTraitの時だとtraitとしての変換
          // classの時だとclassとしての変換だが、traitは総なめをして自classだけで実装するもののみ実装する
          // 依存クラスもリストアップし、AMDに備える
          convertClassDef(cdef)

        case vdef: ValDef =>
          // メンバかどうかでthisつけるか変わる、代入元がemptyかどうかでnullか変わる
          NsEmpty()

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

        case aply: Apply => NsEmpty()

        case block: Block =>
          // 上から代入とかreturnとか修飾子が来てたらそれを最後の評価式につけるだけ
          NsEmpty()

        case m: Match => NsEmpty()
        case ifBlock: If => NsEmpty()
        case Try(block, catches, finalizer) => NsEmpty()
        case select: Select => NsEmpty()
        case ident: Ident => NsEmpty()
        case nw: New => NsEmpty()
        case literal: Literal => NsEmpty()
        case sper: Super if (!SystemClasses.isSystemClass(sper.symbol.superClass.name.toString)) =>
          // 親クラスを直接指定
          NsEmpty()

        case t: Throw => NsEmpty()

        case t: Tree =>
          NsEmpty()

      }
    }

    def convertDoWhile() = {}

    def convertIfElse() = {}

    def convertFor() = {}

    def convertTryCatchFinally() = {}

    def convertBlock() = {}

    def convertCollectionDef() = {}

    def convertClassDef(cdef: ClassDef): NsEmpty = {
      NsClassDef(getFullName(cdef), convertConstructorDef(cdef))
    }

    def convertObjectDef() = {}

    def convertTraitDef() = {}

    def convertMethodDef() = {}

    def convertPropertyDef() = {}

    /**
     * パッケージ宣言とその下に付くクラスひと通り
     *
     * @param pdef
     * @param scopedVars
     */
    def convertPackageDef(pdef: PackageDef, scopedVars: ScopedVariables): NsEmpty = {
      getPackageName(pdef.symbol, None) match {
        case Some(name) => NsList(NsNamespaceDef(name) :: pdef.stats.map(convertTree(_, scopedVars, false)), false)
        case None => NsList(pdef.stats.map(convertTree(_, scopedVars, false)), false)
      }
    }

    def convertImplicit() = {}

    def convertVariableDef() = {}

    def convertValueDef() = {}

    def convertApply() = {}

    def convertOperator() = {}

    def convertConstructorDef(cdef: ClassDef): NsConstructorDef = {
      val newScopedVars = new ScopedVariables(null)
      val params: Map[String, String] = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString.trim() == "<init>") => toParameterNames(ddef.vparamss.head, newScopedVars)
      }.getOrElse(Map[String, String]())

      val memberNames: Map[String, String] = cdef.impl.body.collect {
        case vdef: ValDef =>
          vdef.name.toString.trim() -> PrimitiveTypes.toPrimitiveType(vdef.tpt.toString)
      }.toMap

      val callSuperClass = cdef.impl.body.collectFirst {
        case ddef: DefDef if (ddef.name.toString == "<init>" && !BaseClasses.isBaseClass(ddef.symbol.enclClass.superClass.name.toString)) =>
          ddef.rhs match {
            case b: Block => b.stats.collect {
              case aply: Apply =>
                convertTree(aply, newScopedVars, false) match {
                  case aply: NsCall => aply.toSuperConstructorCall()
                  case _ => NsEmpty()
                }
            }

            case _ => Nil
          }
      }.getOrElse(Nil)

//      val callSuperTraits = superTraitNames.map {
//        name =>
//          NoMoreScriptApply(NoMoreScriptSelect("call", NoMoreScriptIdent(name, false)), List(NoMoreScriptIdent("this", false)), false, false)
//      }

      val bodies = cdef.impl.body.filter {
        case ddef: DefDef => false
        case tree: Tree => true
      }.map(convertTree(_, newScopedVars, false))

      // NoMoreScriptConstructor(name, params, memberNames, callSuperClass ++ callSuperTraits ++ bodies)
      NsConstructorDef(getFullName(cdef))
    }

    def convertPatternMatch() = {}

    def convertLiteral() = {}
  }

}
