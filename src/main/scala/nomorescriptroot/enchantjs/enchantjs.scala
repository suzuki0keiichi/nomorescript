/**
 * enchant.jsの書き方を出来るようにするもの
 * 動作確認目的に使ってしまったため実際のクラスライブラリと違う仕様になっている場所がほとんどです
 * また、実際にはnomorescriptのjarではなく、別途宣言する形が望ましいと考えています
 */
package nomorescriptroot.enchantjs

import com.github.suzuki0keiichi.nomorescript.bridge.JsFunction0
import com.github.suzuki0keiichi.nomorescript.annotation.mock
import com.github.suzuki0keiichi.nomorescript.annotation.global

@mock class Image {
}

@mock class Sprite(var x: Int, var y: Int) {
  var frameIndex = 0
  var frame = 0
  var vx = 0
  var vy = 0
  var scaleX = 0
  var scaleY = 0
  var width = 0
  var height = 0
  var image: Image = null

  var onenterframe: JsFunction0[Sprite, Unit] = null
  def moveTo(x: Int, y: Int) = {}
}

@mock class Scene {
  var backgroundColor = "none"

  def addChild(obj: Any) = {}
}

@mock class Game {
  var onload: JsFunction0[Game, Unit] = null
  var rootScene = new Scene()
  var width = 0
  var height = 0
  var frame = 0
  var assets: Map[Any, Image] = null

  def preload(url: String) = {}
  def start() = {}
}

@global @mock object enchant {
  def apply() = {}
}
