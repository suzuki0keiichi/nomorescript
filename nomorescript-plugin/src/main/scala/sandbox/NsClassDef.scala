package sandbox

case class NsClassDef(name: String, constructorDef: NsConstructorDef) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    constructorDef.toJs(option)
  }
}
