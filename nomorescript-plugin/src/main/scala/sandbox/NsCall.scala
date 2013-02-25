package sandbox

case class NsCall() extends NsEmpty {
  def toSuperConstructorCall(): NsEmpty = NsEmpty()
}
