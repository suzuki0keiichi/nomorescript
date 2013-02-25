package sandbox

import tools.nsc.Global
import tools.nsc.plugins.{PluginComponent, Plugin}

class SandboxPlugin(val global: Global) extends Plugin {
  val name: String = "nomorescript"
  val description: String = "scala to javascript convert plugin"
  var srcDir = ""
  var outputDir = ".." + java.io.File.separator + "js"
  lazy val components: List[PluginComponent] = List(new SandboxPluginComponent(global, this))

  override def processOptions(options: List[String], error: String => Unit) {
    options.foreach {
      case s: String if (s.startsWith("d:")) =>
        outputDir = s.substring(2)

      case s: String if (s.startsWith("s:")) =>
        srcDir = s.substring(2)

      case _ =>
    }
  }
}