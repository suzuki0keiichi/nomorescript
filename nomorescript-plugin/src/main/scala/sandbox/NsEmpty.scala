package sandbox

case class NsEmpty() {
  def toJs(implicit option: ConvertOptions): List[String] = Nil
}