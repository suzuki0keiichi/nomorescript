/**
 * ブラウザで動くJSの書き方を出来るようにするもの
 * 動作確認目的に使ってしまったため実際のクラスライブラリと違う仕様になっている場所がほとんどです
 */
package nomorescriptroot.browser_fortest

import com.github.suzuki0keiichi.nomorescript.annotation.global
import com.github.suzuki0keiichi.nomorescript.annotation.mock
import com.github.suzuki0keiichi.nomorescript.bridge.JsFunction0

@global @mock object window {
  def alert(message: Any) = {}
  def confirm(message: Any) = true
  def prompt(message: Any) = ""
  def setTimeout(func: JsFunction0[Unit, Unit], time: Int) = 0
  def setInterval(func: JsFunction0[Unit, Unit], time: Int) = 0
  var onload: JsFunction0[AnyRef, Unit] = null
}
