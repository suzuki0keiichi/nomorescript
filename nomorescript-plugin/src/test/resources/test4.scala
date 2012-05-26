import root.browser.window._
import com.github.suzuki0keiichi.nomorescript.annotation._

class Animal(val name: String) {
  def call(message: String) {
    alert(message + " " + name)
  }
}

@global object Main {
  def begin() {
    val cat = new Animal("cat")
    val dog = new Animal("dog")

    cat.call("hello")
    dog.call("hey")
  }
}