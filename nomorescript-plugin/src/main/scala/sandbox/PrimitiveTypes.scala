package sandbox

object PrimitiveTypes {
  val types = Map("Int" -> "number", "java.lang.String" -> "string", "String" -> "string", "Any" -> "object")

  def get(name: String) = types.get(name)

  def toPrimitiveType(typeName: String) = {
    if (typeName.indexOf("=>") != -1) {
      "Function"
    } else {
      get(typeName).getOrElse(typeName)
    }
  }
}