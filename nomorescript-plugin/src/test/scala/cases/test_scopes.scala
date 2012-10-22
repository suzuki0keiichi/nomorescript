package test_scopes

import scala.collection.mutable.ListBuffer

import com.github.suzuki0keiichi.nomorescript.annotation._
import Global._

@mock class Console {
  private val _messages = ListBuffer[String]()

  def messages = _messages.toList
  def log(message: String) = { _messages += message }
  def clear() = _messages.clear()
}

@mock class GlobalBase {
  lazy val console_dummy: Console = new Console()
}

@global @mock object Global extends GlobalBase {
}

class ScopeTest {
  val a = 10
  console_dummy.log("a = " + a)

  def hoge() = {
    val a = 20
    console_dummy.log("a = " + a)
  }

  hoge()

  console_dummy.log("a = " + a)

  if (a == 10) {
    val a = 30
    console_dummy.log("a = " + a)
    if (a == 30) {
      val a = 40
      console_dummy.log("a = " + a)
    }
    console_dummy.log("a = " + a)
  }

  console_dummy.log("a = " + a)
}

@global object Main extends App {
  new ScopeTest()
}