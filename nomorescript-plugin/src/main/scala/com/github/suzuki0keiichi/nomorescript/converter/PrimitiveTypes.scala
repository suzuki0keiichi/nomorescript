package com.github.suzuki0keiichi.nomorescript.converter

object PrimitiveTypes {
  val types = Map("Int" -> "number", "String" -> "string", "Any" -> "object")

  def get(name: String) = types.get(name)

  def toPrimitiveType(typeName: String) = {
    if (typeName.indexOf("=>") != -1) {
      "Function"
    } else {
      get(typeName).getOrElse(typeName)
    }
  }
}