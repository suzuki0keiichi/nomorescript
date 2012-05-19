class Global1 {
  println("hello Global1")
}

case class Case1(a: String, b: Int)

class MatchTestClass {
  def matchTestDef(a: Any) = {
    a match {
      case Case1(a, b) =>
        println(a + b)
        true
      case s: String => true
      case _ => false
    }
  }
}

package com.github.suzuki0keiichi.compilertest {

  trait Trait1 {
    def trait1Def() = "trait"
  }

  class Class1 extends Trait1 {
    println("Class1 say hello")

    def class1Def(param1: Int, param2: String) = {
      param1 * 10 / class1DefInt()
      println("class1Def say " + param1 + param2 + trait1Def())
    }

    def class1DefInt() = 10
  }

  class Class2(val val1: Int, var var1: String) {
    def class2Def(param1: Int)(f1: () => Unit) = {
      f1()
      println(param1)
    }
  }

  class Class3(val val1: String, val val2: Any) {
    class Inner1(val innerVal1: String) {
      println(val1 + innerVal1)
    }

    new Inner1("innerClass1 hello")

    def class3Def() = {
      this.val2 match {
        case Some(abc) => abc
        case efg: String => efg
        case None => ""
        case _ => ""
      }
    }
  }

  class Class4(val a: Int) {
    def this(b: Int, c: Int) = {
      this(b + c)
    }
  }

  object Object1 {
    println("hello Object1")
    def object1Def(param1: Int) = {
      println(param1)
    }
  }

  package childpackage {
    class ChildPackageClass1 {
      def childPackageClass1Def() = {
        println("childPackageClass1Def")
      }
    }
  }
}