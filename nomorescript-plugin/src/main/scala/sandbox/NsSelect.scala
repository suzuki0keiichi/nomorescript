package sandbox

import com.github.suzuki0keiichi.nomorescript.trees.Util

case class NsSelect(name: String, child: NsEmpty, prefixModifier: NsPrefixModifier, postfixModifier: NsPostfixModifier) extends NsEmpty {
  override def toJs(implicit option: ConvertOptions): List[String] = {
    val childJs = child.toJs(option)

    val js =
      if (name == "<init>") {
        postfixModifier.modify(childJs)
      } else if (childJs == Nil) {
        List(postfixModifier.modify(name))
      } else {
        postfixModifier.modify(Util.addLast(childJs, "." + name))
      }

    prefixModifier.modify(js)
  }
}
