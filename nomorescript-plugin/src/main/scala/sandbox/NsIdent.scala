package sandbox

case class NsIdent(name: String, prefixModifier: NsPrefixModifier = NsPrefixModifier()) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    List(prefixModifier.modify(name))
  }
}
