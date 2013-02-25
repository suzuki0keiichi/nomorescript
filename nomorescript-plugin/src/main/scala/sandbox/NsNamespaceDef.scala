package sandbox

import collection.mutable.ListBuffer

case class NsNamespaceDef(name: String) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    val js = ListBuffer[String]()

    name.split("\\.").reduceLeft { (total, current) =>
      js.append("if (!" + total + ") " + total + " = {};")
      total + "." + current
    }

    js.append("if (!" + name + ") " + name + " = {};")
    js.append("")
    js.toList
  }
}
