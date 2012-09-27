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
          List(fullName + ".prototype = new " + parent + "__dummy_constructor__();",
            "")
        case _ => Nil
      }) :::
      (traitImplementedMethods match {
        case methods if (methods.isEmpty) => Nil
        case _ => traitImplementedMethods.flatMap(methods =>
          methods._2.map(method => fullName + ".prototype." + method + " = " + methods._1 + ".__impl__." + method + ";")).toList ::: List("")
      }) :::
      children.flatMap(_._2.toJs(true)).toList :::
      List("/**", " * @constructor", " */", fullName + "__dummy_constructor__ = function() {};", fullName + "__dummy_constructor__.prototype = " + fullName + ".prototype;", "")
  }
}

