package sandbox

import com.github.suzuki0keiichi.nomorescript.trees.Util

case class NsPrefixModifier(modifier: Option[String] = None) {
  def modify(result: String) = {
    modifier match {
      case Some(modifier) => modifier + result
      case None => result
    }
  }

  def modify(results: List[String]) = {
    modifier match {
      case Some(modifier) => Util.addFirst(results, modifier)
      case None => results
    }
  }
}
