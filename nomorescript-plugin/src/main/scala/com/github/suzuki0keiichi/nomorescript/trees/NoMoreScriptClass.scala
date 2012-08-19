package com.github.suzuki0keiichi.nomorescript.trees

/**
 * TODO:親クラス(extends)とtrait(with)を分ける
 * TODO:メンバー宣言とコンストラクタ処理を分ける
 */
case class NoMoreScriptClass(
  name: String,
  namespace: Option[String],
  constructor: NoMoreScriptConstructor,
  parent: Option[String],
  traits: List[String],
  traitImplementedMethods: Map[String, List[String]],
  members: Map[String, String],
  children: Map[String, NoMoreScriptTree]) extends NoMoreScriptTree {

  override def toJs(terminate: Boolean) = {
    val fullName = namespace.map(_ + ".").getOrElse("") + name

    // TODO:無名関数実行のスタイルをやめ、普通に名前空間を汚す形にする(Closureが理解できないため)
    // TODO:実際のコンストラクタにはメンバー宣言のみをし、constructorの方は__init__のような関数にしておく
    // TODO:extendsのものはnewで渡し、withのものは実体があるfunctionのみ直接代入する
    List("/**", " * @constructor") :::
      members.map(member => " * @property {" + member._2 + "} " + member._1).toList :::
      parent.map(p => List(" * @extends {" + p + "}")).getOrElse(Nil) :::
      traits.map(t => " * @implement {" + t + "}") :::
      List(" */") :::
      (fullName + " = function() {") ::
      members.map("  this." + _._1 + " = null;").toList :::
      List("}", "") :::
      (parent match {
        case Some(parent) => List(fullName + ".prototype = new " + parent + "();", fullName + ".prototype.__super__ = " + parent + ";", "")
        case _ => Nil
      }) ::: (traits match {
        case list: List[String] if (!list.isEmpty) => List(fullName + ".prototype.__super_traits__ = Array(" + list.mkString(", ") + ");", "")
        case _ => Nil
      }) ::: constructor.toJs(true) :::
      (traitImplementedMethods match {
          case methods if (methods.isEmpty) => Nil
          case _ => traitImplementedMethods.flatMap(methods =>
            methods._2.map(method => fullName + ".prototype." + method + " = " + methods._1 + ".prototype." + method + ";")).toList ::: List("")
        }) :::
        children.flatMap(_._2.toJs(true)).toList
  }
}

