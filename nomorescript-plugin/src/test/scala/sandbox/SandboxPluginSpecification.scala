package sandbox

import org.specs2.mutable.Specification
import io.Source
import java.io.File

class SandboxPluginSpecification extends Specification {
  private lazy val projectRootDir = {
    val currentPath = getClass.getResource("").getFile
    val index: Int = currentPath.indexOf("target" + File.separator + "scala-2.9.2") match {
      case -1 => currentPath.indexOf("target/scala-2.9.2")
      case index: Int => index
    }

    if (index != -1) {
      currentPath.substring(0, index)
    } else {
      currentPath
    }
  }

  private val outputRootDir = (new File(projectRootDir + "/target/")).getAbsolutePath()
  private val srcRootDir = (new File(projectRootDir + "/src/test/resources/")).getAbsolutePath()
  private val testScriptDir = (new File(srcRootDir + "/sandbox/")).getAbsolutePath()

  private def compile(scalaFileName: String, jsFileName: String) = {
    val compiler = new TestCompiler(List("s:", "d:.." + File.separator + ".." + File.separator + "test-js"))

    fileDelete(outputRootDir + "/test-js/sandbox/" + jsFileName)
    compiler.compile(srcRootDir, outputRootDir + "/scala-2.9.2/test-classes")(testScriptDir + "/" + scalaFileName)
  }

  private def diff(actualFileName: String, expectedFileName: String) = {
    val src1 = Source.fromFile(outputRootDir + "/test-js/sandbox/" + actualFileName).getLines().toList.map(_ + "\n")
    val src2 = Source.fromFile(testScriptDir + "/" + expectedFileName).getLines().toList.map(_ + "\n")

    src2 mustEqual src1
  }

  private def fileExists(name: String) = {
    val file = new File(name)

    file.exists()
  }

  private def fileDelete(name: String) = {
    val file = new File(name)

    if (file.exists()) {
      file.delete()
    }
  }

  private def convertAndDiff(fileName: String) = {
    val reporter = compile(fileName + ".scala", fileName + ".js")
    if (reporter.infos.size > 0) {
      (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
    }

    reporter.hasErrors must beFalse

    diff(fileName + ".js", fileName + ".js")
  }

  "package.scala" in (convertAndDiff("package"))
  "class.scala" in (convertAndDiff("class"))
  "field.scala" in (convertAndDiff("field"))
  "constructor.scala" in (convertAndDiff("constructor"))
  "method.scala" in (convertAndDiff("method"))
}
