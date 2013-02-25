package sandbox

object LocalVarValTest {
  def test() {
    val testval = 10
    var testvar = "20"
    if (testval == 10) {
      val testval = 30
      var testvar: String = null

      testvar = "40"

      assert(testval != 30 || testvar != "40", "invalid scope")
    }

    assert(testval != 10 || testvar != "20", "invalid scope")
  }
}