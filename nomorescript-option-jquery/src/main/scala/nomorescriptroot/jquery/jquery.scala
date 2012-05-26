/**
 * jqueryの書き方を出来るようにするもの
 * 動作確認目的に使ってしまったため実際のクラスライブラリと違う仕様になっている場所がほとんどです
 * また、実際にはnomorescriptのjarの中に含まれるのではなく、別途宣言する形が望ましいと考えています
 */
package nomorescriptroot

import com.github.suzuki0keiichi.nomorescript.annotation.mock

package object jquery {
  def $(key: String): nomorescriptroot.jquery.DOM = {
    null
  }
}

package nomorescriptroot.jquery {
  @mock trait DOM {
    def add(expr: String)
  }
}
