package sandbox

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.io.PlainFile
import java.net.URLClassLoader

class TestCompiler(options: List[String]) {
  def getSettings(srcDir: String, outputDir: String) = {
    val settings = new Settings()

    settings.classpath.value = {
      val loader = classOf[SandboxPlugin].getClassLoader.asInstanceOf[URLClassLoader]
      val entries = loader.getURLs.map(_.getPath.replaceAll("%20", " ")) // Windowsだとスペース対策をしないとクラスパスとして正しく扱われない

      ClassPath.join(entries: _*)
    }

    settings.deprecation.value = true
    settings.unchecked.value = true
    settings.outputDirs.add(PlainFile.fromPath(srcDir), PlainFile.fromPath(outputDir))
    settings
  }

  def compile(srcDir: String, outputDir: String)(pathes: String*) = {
    val global = new TestGlobal(options, getSettings(srcDir, outputDir))
    val run = new global.Run

    run.compile(pathes.toList)
    global.storeReporter
  }
}

class TestGlobal(options: List[String], settings: Settings, val storeReporter: StoreReporter = new StoreReporter()) extends Global(settings, storeReporter) {
  lazy val plugin = new SandboxPlugin(this)

  plugin.processOptions(options, { message => })

  override protected def computeInternalPhases() {
    plugin.components foreach this.phasesSet.+=
    super.computeInternalPhases()
  }
}
