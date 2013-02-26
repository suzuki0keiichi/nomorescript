class MethodTest {
  def method1(param1: String) = {
    println(param1)
    10
  }

  def method2(param2: String) = {
    method1(param2)
  }

  def method3(param3: String = "hello") = {
    method2(param3)
  }

  method3()
}