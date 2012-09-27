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

    constructor.toJsForInterface() :::
      children.flatMap {
        child =>
          child._2 match {
            case d: NoMoreScriptTraitDef => d.toJsForInterface()
            case _ => Nil
          }
      }.toList :::
      (children.flatMap(_._2.toJs(true)).toList)
  }
}
