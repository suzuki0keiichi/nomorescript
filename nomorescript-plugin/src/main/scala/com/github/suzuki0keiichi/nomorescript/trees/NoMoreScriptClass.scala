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

    List("/**", " * @constructor") :::
      members.map(member => " * @property {" + member._2 + "} " + member._1).toList :::
      parent.map(p => List(" * @extends {" + p + "}")).getOrElse(Nil) :::
      traits.map(t => " * @implements {" + t + "}") :::
      constructor.params.map(param => " * @param {" + param._2 + "} " + param._1).toList :::
      List(" */") :::
      constructor.toJs(true) :::
      (parent match {
        case Some(parent) =>
          List("var __DummyClass__ = function(){};", "",
            "__DummyClass__.prototype = " + parent + ".prototype;",
            fullName + ".prototype = new __DummyClass__();",
            fullName + ".prototype.__super__ = " + parent + ";",
            "")
        case _ => Nil
      }) ::: {traits match {
      case Nil => Nil
      case list: List[_] if (!list.isEmpty) => List(fullName + ".prototype.__super_traits__ = Array(" + traits.mkString(", ") + ");", "")
    }} :::
      (traitImplementedMethods match {
        case methods if (methods.isEmpty) => Nil
        case _ => traitImplementedMethods.flatMap(methods =>
          methods._2.map(method => fullName + ".prototype." + method + " = " + methods._1 + ".__impl__." + method + ";")).toList ::: List("")
      }) :::
      children.flatMap(_._2.toJs(true)).toList
  }
}

