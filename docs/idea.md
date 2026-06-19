# アイディア帳

- [syntax.md](syntax.md)
- [annotation.md](annotation.md)



- 木の操作フレームワークを調べる
    - XSL
    - RELAX NG
        - 生け垣オートマトン
- LuaLSのアノテーションのくっつきかた問題
    - `doc.json`から読み取れる？
    - LuaLSと直接話せばよいのでは？
- 代表トークンとソース位置を分離する
    - LuaLSのアノテーションのくっつきかたを真似するために行番号が必要
    - 行アノテーション: 次の行かどうかを調べたい
    - 式アノテーション: 式の最右ノードが同じ行にあるかどうかを調べたい
- 変数宣言と定義の構文木上の順序入れ替え
- matcherの関数をどこかに移してもよいかも（utilに移すと依存関係がややこしい？）
- parserを作成するお役立ち関数を作ってもよいかも
- nodeのkindへの追加を検討する
    - field: 名前の束縛だとannotationがくっつける
        - むしろbindとかそういうのが欲しい？
    - clause: とりあえずif用
- 参考文献: https://blog.miz-ar.info/2025/09/surface-syntax/
- classとかgenericなどの継承のことを考える
- エラー関係
    - エラーメッセージの構築方法を考える
        - to_stringじゃなくてmake_error_stringにする
    - テストの出力を洗練する
    - 網羅性について考える
        - 文法エラーのパターンをどれくらい考えるべきか
        - Luaがバージョンアップしたらどうする？



## ごみ箱

### 2026/06/19

- Luaパーサのエラーメッセージをもうちょっとがんばる
    - `require`の引数を調整したほうが良いかも
    - 統一性のために`check`も？
    - とはいえ、`check "kind"`という呼び出しかたは良いと思う
    - `@overload`するか、`append/extend`みたいな述語をさがす？
    - `require_or`が採用された
    - Luaのエラーを調査する
- エラー関係
    - `lparser.c`
        - `check`
            - `error_expected`
                - `"%s expected", luaX_token2str(...)`
            - `check_match`
    - `Name`をexpectするときって`<name>`な気がするけどどうしようか。
        - `luaX_token2str`を参考に実装
    - `near`の実装
        - レキサのnearは実装しない
    - `unexpected symbol`と`syntax error`の区別
        - `unexpected symbol`が出てくるのは`primaryexp()`
            - `primaryexp -> NAME | '(' expr ')'`
        - `syntax error`が出てくるのは`exprstat()`と`restassign()`
        - `node:require_or`は引数に`near`用のトークンを渡すべき
    - tablesepのパースエラーがどうなるかを調べる
    - is_stat_terminalとis_varをどうするか考える

### 2026/06/18

- `'for' Name ('=' | 'in' | ',')`で3個めのトークンをチェックする

### 2026/06/17

- operatorの構文をチェックする
    - https://github.com/LuaLS/lua-language-server/issues/599#issuecomment-881959977

### 2026/06/15

- `type[][]`は可能だろうけど、`type??`は可能なのか？
    - 可能
- 返り値の型リストは括弧で囲めるか？
    - やっぱり囲める
        - `fun ():(integer, string, boolean)`
        - `fun ():(integer, (string), boolean)`
            - これも可能だけど、内側の`(string)`は1要素のunionだと思う
    - 仮引数の型リストと同じなんじゃない？
        - 仮引数の場合は変数名が必須
    - CPSみたいなかんじ？

### 2026/06/08

- annotationをdocにするか……
    - テスト名も変える
- @asを正しく扱うために、コメント開始部分からアノテーションレキサに移譲する
- もしくは、lex関数をふたつ用意する: lex_expression, lex_line
    - doc_lexer.lex, doc_lexer.lex_expression
- ~~updateはupdate_srclocに戻したほうがわかりやすい？~~

### 2026/06/05

- 文字列長の降順で並びかえる処理はmatcherに切り出してもよいかも
- カンマとかセパレーターを律儀に`node:update`しなくてよい

### 2026/05/31

- 代表トークンとソース位置を分離する
    - ~~終端ノード以外は~~開始位置と終了位置を持つ必要がある
        - 複数行文字列を考慮するとトークンも開始位置と終了位置が必要
        - [x] `matcher`に`source_location:update`を移すことを検討
        - `source_location:to_string`でなく`source_location:make_error_string`を作る？
        - [x] `source_location`を`cloneす`るタイミングをよくかんがえる
            - immutableにしたほうがよいかも: Lua的にはmetatableで制御するくらいしかない
                - 静的解析をそのうち実装するのがかっこいいかも
        - パースするときに左の端は既にわかっている
        - ノードが完成したときには右の端もわかる
        - どんなインターフェースで設定するのがかっこいいか
            - e.g. `if ... else ... end`がどうなるかを考えてみる
            - `source_location`を比較すればインターフェースは楽になるけど本質的には不要なはず
        - EOFトークンはノードに含まれないので、ノードの`last_srcloc`は非`nil`
        - 空ブロックのコード位置が決まらない問題
            - 論理的に正しいよね
        - なにも返さないreturn文の式リストも
    - LuaLSのアノテーションのくっつきかたを真似するために行番号が必要
    - 行アノテーション: 次の行かどうかを調べたい
    - 式アノテーション: 式の最右ノードが同じ行にあるかどうかを調べたい
- annotation_lexerのアノテーションマッチはこのままだと@paramXXX`もマッチしちゃうので注意
    - `@as`をインラインだけにするのを忘れないこと

### 2026/05/24

- 行内の位置はバイト単位とコードポイント位置のどちらがよいか？
    - バイト単位
- matcher:match_short_stringのエラー時のふるまいとメッセージを確認する
    - invalid escape sequenceになる場合
    - quotationで終端しない場合: EOF, 生の改行