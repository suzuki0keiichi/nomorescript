package com.github.suzuki0keiichi.nomorescript

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import compiler.TestCompiler
import scala.io.Source
import trees._
import javax.script.ScriptEngineManager
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class NoMoreScriptPluginSpecification extends Specification {
  lazy val currentPath = getClass.getResource("").getFile
  lazy val srcRoot = {
    val path = getClass.getResource("").getFile

    path.substring(0, path.length() - "/com/github/suzuki0keiichi/nomorescript".length())
  }

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

  "total" should {
    "test1.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))

      fileDelete("target/test-js/com/github/suzuki0keiichi/nomorescript/test1.txt.js")

      val reporter = compiler.compile(currentPath + "test1.scala.txt")
      if (reporter.infos.size > 0) {
        reporter.infos.head.toString mustEqual ""
      }

      reporter.hasErrors must beFalse

      val src1 = Source.fromFile("target/test-js/com/github/suzuki0keiichi/nomorescript/test1.txt.js").getLines().toList.map(_ + "\n")
      val src2 = Source.fromFile(currentPath + "test1.js").getLines().toList.map(_ + "\n")

      src2 mustEqual src1
    }

    "test2.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))

      fileDelete("target/test-js/com/github/suzuki0keiichi/nomorescript/test2.js")
      val reporter = compiler.compile(currentPath + "test2.scala.txt")

      reporter.hasErrors must beTrue
      reporter.infos.size mustEqual 7

      (new java.io.File("target/test-js/com/github/suzuki0keiichi/nomorescript/test2.js")).exists() mustEqual false
    }

    "test3.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))

      fileDelete("target/test-js/com/github/suzuki0keiichi/nomorescript/test3.txt.js")
      val reporter = compiler.compile(currentPath + "test3.scala.txt")
      if (reporter.infos.size > 0) {
        reporter.infos.head.toString mustEqual ""
      }

      reporter.hasErrors must beFalse

      val src1 = Source.fromFile("target/test-js/com/github/suzuki0keiichi/nomorescript/test3.txt.js").getLines().toList.map(_ + "\n")
      val src2 = Source.fromFile(currentPath + "test3.js").getLines().toList.map(_ + "\n")

      src2 mustEqual src1
    }

    "test4.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))

      val reporter = compiler.compile(currentPath + "test4.scala.txt")
      if (reporter.infos.size > 0) {
        reporter.infos.head.toString mustEqual ""
      }

      reporter.hasErrors must beFalse

      true
    }

    "test5.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))

      val reporter = compiler.compile(currentPath + "test5.scala.txt")
      if (reporter.infos.size > 0) {
        reporter.infos.head.toString mustEqual ""
      }

      reporter.hasErrors must beFalse

      true
    }

    "test_scopes.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js", "s:" + srcRoot))
      val reporter = compiler.compile(currentPath + "test_scopes.scala.txt")
      if (reporter.infos.size > 0) {
        reporter.infos.head.toString mustEqual ""
      }

      val console = evalJs("target/test-js/com/github/suzuki0keiichi/nomorescript/test_scopes.txt.js")

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
    val file = new java.io.File(name)

    file.exists()
  }

  def fileDelete(name: String) = {
    val file = new java.io.File(name)

    if (file.exists()) {
      file.delete()
    }
  }
}
