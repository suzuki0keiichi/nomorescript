#nomorescriptとは
ScalaのコードをJavaScriptに変換するscalaのコンパイラープラグインです。
s2jsに触発され&コンパイラープラグインを触ってみたい＆Closure Library非依存のものを作ってみたくて作成しました。
非サポートな機能はTODOに書いてあるものと同様です。
どのような形に変換されるかはtest/scala以下にあるspecを見て下さい。
jar分け、試験も不十分ですがしばらく趣味プログラムに時間を取れなさそうなのでコミットだけしてある状態です。
Scala的な書き方でのご指摘、機能追加、テスト追加、不具合修正等お待ちしてます。

#TODO
 * Map.applyのJSON変換
 * pattern matching
 * enchant.js、prototype.jsのクラス生成対応
 * jquery対応
 * dom系対応
 * case class
 * 継承、mix-in
 * 到達しないコードの削除(別phaseを使っていた時の名残です)
 * object singleton対応(@globalをつければglobal(window下など)に配置することはできます
 * return対応のしょぼさ修正(;対応は変換時なのにreturnはクラス生成時などバラバラ)
