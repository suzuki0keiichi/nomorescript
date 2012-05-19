/**
 * jqueryの書き方を出来るようにするもの
 * 動きません
 */
package nomorescriptroot.jquery

import com.github.suzuki0keiichi.nomorescript.annotation.mock

@mock trait DOM {
  def add(expr: String)
}

/**
 * この状態ではうまく$("hoge")という使い方ができませんでした
 */
@mock object $ {
  def apply(key: String): DOM = {
    null
  }
}