package com.github.suzuki0keiichi.nomorescript

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import compiler.TestCompiler
import scala.io.Source
import trees._
import javax.script.ScriptEngineManager
import scala.collection.mutable.ListBuffer
import java.io.File

@RunWith(classOf[JUnitRunner])
class NoMoreScriptPluginSpecification extends Specification {
  lazy val projectRootDir = {
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

  val outputRootDir = (new File(projectRootDir + "/target/")).getAbsolutePath()
  val srcRootDir = (new File(projectRootDir + "/src/test/resources/")).getAbsolutePath()
  val testScriptDir = (new File(srcRootDir + "/com/github/suzuki0keiichi/nomorescript/")).getAbsolutePath()

  class DummyConsole {
    private val rawMessages = ListBuffer[String]()

    def log(message: String) = {
      rawMessages += message
    }

    def messages = rawMessages.toList
  }

  "NoMoreScriptApply" should {
    "0 argument" in {
      NoMoreScriptApply(NoMoreScriptSelect("hogeDef", NoMoreScriptIdent("id", false)), List(), false, false).toJs(false).mkString("_") mustEqual "id.hogeDef()"
    }

    "1 argument" in {
      NoMoreScriptApply(NoMoreScriptSelect("hogeDef", NoMoreScriptIdent("id", false)), List(NoMoreScriptLiteral("10", false)), false, false).toJs(false).mkString("_") mustEqual "id.hogeDef(10)"
    }

    "2 arguments" in {
      NoMoreScriptApply(NoMoreScriptSelect("hogeDef", NoMoreScriptIdent("id", false)), List(NoMoreScriptLiteral("10", false), NoMoreScriptLiteral("\"abc\"", false)), false, false).toJs(false).mkString("_") mustEqual "id.hogeDef(10, \"abc\")"
    }

    "this" in {
      NoMoreScriptApply(NoMoreScriptSelect("hogeDef", NoMoreScriptThis(false)), List(NoMoreScriptSelect("hogeVal", NoMoreScriptThis(false))), false, false).toJs(false).mkString("_") mustEqual "this.hogeDef(this.hogeVal)"
    }
  }

  private def compile(scalaFileName: String, jsFileName: String) = {
    val compiler = new TestCompiler(List("s:", "d:.." + File.separator + ".." + File.separator + "test-js"))

    fileDelete(outputRootDir + "/test-js/com/github/suzuki0keiichi/nomorescript/" + jsFileName)
    compiler.compile(srcRootDir, outputRootDir + "/scala-2.9.2/test-classes")(testScriptDir + "/" + scalaFileName)
  }

  private def diff(actualFileName: String, expectedFileName: String) = {
    val src1 = Source.fromFile(outputRootDir + "/test-js/com/github/suzuki0keiichi/nomorescript/" + actualFileName).getLines().toList.map(_ + "\n")
    val src2 = Source.fromFile(testScriptDir + "/" + expectedFileName).getLines().toList.map(_ + "\n")

    src2 mustEqual src1
  }

  "total" should {
    "test1.scala" in {
      val reporter = compile("test1.scala.txt", "test1.txt.js")
      if (reporter.infos.size > 0) {
        (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
      }

      reporter.hasErrors must beFalse

      diff("test1.txt.js", "test1.js")
    }

    "test2.scala" in {
      val reporter = compile("test2.scala.txt", "test2.txt.js")

      reporter.hasErrors must beTrue
      reporter.infos.size mustEqual 7

      (new File(outputRootDir + "/test-js/com/github/suzuki0keiichi/nomorescript/test2.js")).exists() mustEqual false
    }

    "test3.scala" in {
      val reporter = compile("test3.scala.txt", "test3.txt.js")
      if (reporter.infos.size > 0) {
        (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
      }

      reporter.hasErrors must beFalse

      diff("test3.txt.js", "test3.js")
    }

    "test4.scala" in {
      val reporter = compile("test4.scala.txt", "test4.txt.js")
      if (reporter.infos.size > 0) {
        (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
      }

      reporter.hasErrors must beFalse

      true
    }

    "test5.scala" in {
      val reporter = compile("test5.scala.txt", "test5.txt.js")
      if (reporter.infos.size > 0) {
        (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
      }

      reporter.hasErrors must beFalse

      true
    }

    "test_scopes.scala" in {
      val reporter = compile("test_scopes.scala.txt", "test_scopes.txt.js")
      if (reporter.infos.size > 0) {
        (reporter.infos.head.pos + " " + reporter.infos.head.msg) mustEqual ""
      }

      val console = evalJs(outputRootDir + "/test-js/com/github/suzuki0keiichi/nomorescript/test_scopes.txt.js")

      _root_.test_scopes.Global.console.clear()
      new _root_.test_scopes.ScopeTest()

      _root_.test_scopes.Global.console.messages mustEqual console.messages
    }
  }

  def evalJs(filename: String) = {
    val manager = new ScriptEngineManager()
    val engine = manager.getEngineByName("javascript")
    val src = Source.fromFile(filename)
    val bindings = engine.createBindings()
    val console = new DummyConsole()

    bindings.put("console", console)
    engine.eval(src.bufferedReader(), bindings)

    console
  }

  def fileExists(name: String) = {
    val file = new File(name)

    file.exists()
  }

  def fileDelete(name: String) = {
    val file = new File(name)

    if (file.exists()) {
      file.delete()
    }
  }
}
