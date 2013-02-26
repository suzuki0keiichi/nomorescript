package sandbox

import com.github.suzuki0keiichi.nomorescript.trees.Util

case class NsPostfixModifier(terminate: Boolean = false) {
  def modify(result: String) = {
    if (terminate) {
      result + ";"
    } else {
      result
    }
  }

  def modify(results: List[String]) = {
    if (terminate) {
      Util.addLast(results, ";")
    } else {
      results
    }
  }
}
