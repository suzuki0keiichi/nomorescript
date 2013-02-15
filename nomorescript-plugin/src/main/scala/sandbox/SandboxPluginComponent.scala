package sandbox

import tools.nsc.plugins.PluginComponent
import tools.nsc.{Global, Phase}

/**
 * ポリシーとしてはあくまでJavaScript寄りの中間クラスに変換する
 * そこからjsdocやAMD、CommonJS、ClosureCompiler形式の出力を行う
 */
class SandboxPluginComponent(val global: Global) extends PluginComponent {

  import global._

  val runsAfter: List[String] = List("refchecks")
  val phaseName: String = "scala to javascript convert phase"

  def newPhase(prev: Phase) = new SandboxPhase(prev)

  class SandboxPhase(prev: Phase) extends StdPhase(prev) {

    override def name: String = phaseName

    implicit def nameToString(name: Name) = name.toString

    def isUserClass(cdef: ClassDef) = !cdef.name.startsWith("$anonfun$")

    object SystemClasses {
      val classNames = List("Any")

      def isSystemClass(name: Name) = classNames.contains(name.toString)
    }

    def apply(unit: CompilationUnit) {
      unit.body match {
        case ths: This =>
        // globalアノテーションがついていた場合のみthisを付けない

        case pdef: PackageDef =>
        // パッケージ宣言のみ

        case cdef: ClassDef if (isUserClass(cdef)) =>
          // hasModuleの場合だとglobal的な扱い
          // isTraitの時だとtraitとしての変換
          // classの時だとclassとしての変換だが、traitは総なめをして自classだけで実装するもののみ実装する
          // 依存クラスもリストアップし、AMDに備える

        case vdef: ValDef =>
          // メンバかどうかでthisつけるか変わる、代入元がemptyかどうかでnullか変わる

        case select: Select =>
        case ident: Ident =>
        case nw: New =>
        case fun: Function =>
        case sper: Super if (!SystemClasses.isSystemClass(sper.symbol.superClass.name.toString)) =>
        // 親クラスを直接指定

        case block: Block =>
        // 上から代入とかreturnとか修飾子が来てたらそれを最後の評価式につけるだけ

        case Try(block, catches, finalizer) =>
        case aplyImplicit: ApplyImplicitView =>
          if (isJsFunction(aplyImplicit)) {
            // 第一引数をthisとするfunction
          } else {
            // 変換関数を挟む
          }

        case aply: Apply =>
        case ddef: DefDef if (isUserMethod(ddef)) =>
        case ifBlock: If =>
        case literal: Literal =>
        case t: Throw =>
        case m: Match =>

        case t: Tree =>

      }
    }

    def convertDoWhile() = {}

    def convertIfElse() = {}

    def convertFor() = {}

    def convertTryCatchFinally() = {}

    def convertBlock() = {}

    def convertCollectionDef() = {}

    def convertClassDef() = {}

    def convertObjectDef() = {}

    def convertTraitDef() = {}

    def convertMethodDef() = {}

    def convertPropertyDef() = {}

    def convertPackageDef() = {}

    def convertImplicit() = {}

    def convertVariableDef() = {}

    def convertValueDef() = {}

    def convertApply() = {}

    def convertOperator() = {}

    def convertConstructorDef() = {}

    def convertPatternMatch() = {}

    def convertLiteral() = {}
  }

}
