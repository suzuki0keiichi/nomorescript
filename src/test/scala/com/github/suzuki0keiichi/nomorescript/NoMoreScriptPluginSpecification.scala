package com.github.suzuki0keiichi.nomorescript

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import trees._
import compiler.TestCompiler
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class NoMoreScriptPluginSpecification extends Specification {
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
      val compiler = new TestCompiler(List("d:target/test-js"))

      fileDelete("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test1.js")
      compiler.compile("src/test/resources/compilertest/com/github/suzuki0keiichi/compilertest/test1.scala")

      val src1 = Source.fromFile("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test1.js").getLines().toList
      val src2 = Source.fromFile("src/test/resources/compilertest/com/github/suzuki0keiichi/compilertest/test1.js").getLines().toList

      src2 mustEqual src1
    }

    "test2.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js"))

      fileDelete("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test2.js")
      compiler.compile("src/test/resources/compilertest/com/github/suzuki0keiichi/compilertest/test2.scala")

      (new java.io.File("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test2.js")).exists() mustEqual false
    }

    "test3.scala" in {
      val compiler = new TestCompiler(List("d:target/test-js"))

      fileDelete("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test3.js")
      compiler.compile("src/test/resources/compilertest/com/github/suzuki0keiichi/compilertest/test3.scala")

      val src1 = Source.fromFile("target/test-js/resources/compilertest/com/github/suzuki0keiichi/compilertest/test3.js").getLines().toList
      val src2 = Source.fromFile("src/test/resources/compilertest/com/github/suzuki0keiichi/compilertest/test3.js").getLines().toList

      src2 mustEqual src1
    }
  }

  def fileDelete(name: String) = {
    val file = new java.io.File(name)

    if (file.exists()) {
      file.delete()
    }
  }
}