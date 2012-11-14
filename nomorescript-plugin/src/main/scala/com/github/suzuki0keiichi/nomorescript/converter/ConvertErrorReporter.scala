package com.github.suzuki0keiichi.nomorescript.converter
import scala.tools.nsc.util.Position

trait ConvertErrorReporter {
  def addError(pos: Position, message: String)
}