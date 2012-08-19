package com.github.suzuki0keiichi.nomorescript.trees

/**
 * JsDocにはinterfaceとclassしかないが、traitを実現する
 */
case class NoMoreScriptTrait(
  name: String,
  namespace: Option[String],
  constructor: NoMoreScriptConstructor,
  children: Map[String, NoMoreScriptTree]) extends NoMoreScriptTree {

  override def toJs(terminate: Boolean) = {
    val fullName = namespace.map(_ + ".").getOrElse("") + name

    // TODO:無名関数実行のスタイルをやめ、普通に名前空間を汚す形にする(Closureが理解できないため)
    // TODO:実際のコンストラクタにはメンバー宣言のみをし、constructorの方は__init__のような関数にしておく
    // TODO:extendsのものはnewで渡し、withのものは実体があるfunctionのみ直接代入する
    List("/**", " * @interface", " */", fullName + " = function() {};", "") :::
      children.flatMap { child =>
        child._2 match {
          case d: NoMoreScriptTraitDef => d.toJsForInterface()
          case _ => Nil
        }
      }.toList ::: List("(function() {", "  var __trait__ = " + fullName + ";", "") :::
      constructor.toJsForInterface().map("  " + _) ::: (children.flatMap(_._2.toJs(true)).toList).map("  " + _) ::: List("})();") ::: List("")
  }
}
