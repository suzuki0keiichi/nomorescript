package com.github.suzuki0keiichi.nomorescript.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.Global

class NoMoreScriptPlugin(val global: Global) extends Plugin {
  val name: String = "nomorescript"
  val description: String = "scala to javascript convert plugin"
  var srcRootDir = (new java.io.File("src/main/scala")).getAbsolutePath()
  var outputDir = "target/js"
  lazy val components: List[PluginComponent] = List(new NoMoreScriptPluginComponent(global, this))

  override def processOptions(options: List[String], error: String => Unit) = {
    options.foreach {
      case s: String if (s.startsWith("d:")) =>
        outputDir = s.substring(2)

      case s: String if (s.startsWith("s:")) =>
        srcRootDir = (new java.io.File(s.substring(2))).getAbsolutePath()

      case _ =>
    }
  }
}