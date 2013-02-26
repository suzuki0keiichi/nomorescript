package sandbox

case class NsLiteral(value: String, prefixModifier: NsPrefixModifier = NsPrefixModifier(), postfixModifier: NsPostfixModifier = NsPostfixModifier()) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    List(prefixModifier.modify(postfixModifier.modify(value)))
  }
}
