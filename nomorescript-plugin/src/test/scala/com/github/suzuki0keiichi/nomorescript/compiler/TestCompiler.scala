package com.github.suzuki0keiichi.nomorescript.compiler

import java.net.URL
import java.net.URLClassLoader
import scala.Array.canBuildFrom
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import com.github.suzuki0keiichi.nomorescript.plugin.NoMoreScriptPlugin
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter

class TestCompiler(val options: List[String] = Nil) {
  val outputDir = new VirtualDirectory("[memory]", None)

  lazy val settings = {
    val settings = new Settings()

    settings.classpath.value = {
      val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
      val entries = loader.getURLs.map(_.getPath.replaceAll("%20", " ")) // Windowsだとスペース対策をしないとクラスパスとして正しく扱われない

      ClassPath.join(entries: _*)
    }

    settings.deprecation.value = true
    settings.unchecked.value = true
    settings.outputDirs.setSingleOutput(outputDir)
    settings
  }

  def compile(pathes: String*) = {
    val global = new TestGlobal(options, settings)
    val run = new global.Run

    run.compile(pathes.toList)
  }
}

class TestGlobal(options: List[String], settings: Settings) extends Global(settings, new StoreReporter()) {
  lazy val plugin = new NoMoreScriptPlugin(this)

  plugin.processOptions(options, { message => })

  override protected def computeInternalPhases() {
    plugin.components foreach this.phasesSet.+=
    super.computeInternalPhases()
  }
}
