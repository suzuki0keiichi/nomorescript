package sandbox

import scala.tools.nsc.symtab.Symbols

class ScopedVariables(private val parent: ScopedVariables) {
  private val vars = scala.collection.mutable.Map[String, Symbols#Symbol]()

  private def exists(name: String): Boolean = {
    if (vars.contains(name)) {
      true
    } else {
      false
    }
  }

  private def renamePut(origName: String, sym: Symbols#Symbol, count: Int): String = {
    val newName = origName + "__scoped__" + count

    if (exists(newName)) {
      renamePut(origName, sym, count + 1)
    } else {
      put(newName, sym)
    }
  }

  def put(name: String, sym: Symbols#Symbol): String = {
    if (exists(name)) {
      renamePut(name, sym, 1)
    } else {
      vars.put(name, sym)
      name
    }
  }

  def getName(sym: Symbols#Symbol): String = {
    val currentResult = vars.collectFirst {
      case (name, varsSym) if (varsSym == sym) => name
    }

    currentResult match {
      case Some(name) => name
      case None => if (parent != null) { parent.getName(sym) } else { sym.name.toString() }
    }
  }
}
