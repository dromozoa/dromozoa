# アノテーション

## 仕様

- https://luals.github.io/wiki/annotations/
- https://github.com/LuaLS/LuaLS.github.io/blob/main/src/content/wiki/annotations.mdx
    - [annotations.mdx](luadoc/annotations.mdx)
- https://github.com/LuaLS/lua-language-server/blob/master/script/parser/luadoc.lua
    - [luadoc.lua](luadoc/luadoc.lua)
- https://github.com/tree-sitter-grammars/tree-sitter-luadoc/blob/master/grammar.js
    - [grammar.js](luadoc/grammar.js)

## アノテーション

| アノテーション |
|----------------|
| `@alias`       |
| `@as`          |
| `@async`       |
| `@cast`        |
| `@class`       |
| `@deprecated`  |
| `@diagnostic`  |
| `@enum`        |
| `@field`       |
| `@generic`     |
| `@meta`        |
| `@module`      |
| `@nodiscard`   |
| `@operator`    |
| `@overload`    |
| `@package`     |
| `@param`       |
| `@private`     |
| `@protected`   |
| `@public`      |
| `@return`      |
| `@see`         |
| `@source`      |
| `@type`        |
| `@vararg`      |
| `@version`     |

## 字句解析

- アノテーション
    - コメントの先頭にしか出現できない
    - Luaのパターン:
        1. `%-%-%-%s*@`
        2. `%-%-%[=*%[%s*@`
    - `@`の後は`Name`が来る模様
    - luaのレキサでも検査するか？
        - 少なくとも`--[[@as type]]`は検査が必要
- `Integer`
    - 十進整数の即値
    - Luaのパターン: `%-?%d+`
    - 例:
        - `0|1|string`
        - `{ [1]: boolean, [2]: string? }`
- `String`
    - Luaの文字列リテラル規則に従う
- `Name`の範囲が広い
    - Luaのパターン:
        1. `Integer`の検査前: `%d+%.[%w_.*%-\x80-\xFF]*`
        2. `Integer`の検査後: `[%w_\x80-\xFF][%w_.*%-\x80-\xFF]*`
- `Code`
    - Luaのパターン: `` `[^`]*` ``
- `Symbol`
    - 記号表を参照

### 記号表

| 記号                | 否定先読み (PCRE) | 備考                       |
| ------------------- | ----------------- | -------------------------- |
| `:`                 |                   | 型指定                     |
| <code>&#x7C;</code> |                   |                            |
| `,`                 |                   |                            |
| `;`                 |                   | フィールドセパレーター     |
| `<`                 |                   | ジェネリクス               |
| `>`                 |                   | ジェネリクス               |
| `(`                 |                   |                            |
| `)`                 |                   |                            |
| `?`                 |                   |                            |
| `+`                 |                   | `@cast`の型追加            |
| `#`                 |                   | コメント開始               |
| `{`                 |                   |                            |
| `}`                 |                   |                            |
| `*`                 |                   | `Name`内にしか出現しない？ |
| `[]`                |                   |                            |
| `...`               |                   | 特殊な`Name`扱いでよさそう |
| `[`                 |                   |                            |
| `]`                 |                   |                            |
| `-`                 | `-(?!-)`          | `@cast`の型除去            |
| `.`                 | `\.(?!\.)`        | `Name`内にしか出現しない？ |
| `@`                 |                   | コメント開始               |
| `--`                |                   | コメント開始               |

## 構文解析

- 空のタプルは許容されなかった
    - タプルは名前付きの型を持てない
- 空のテーブルは許容されたが、意味は`table`になった
- 仮引数の名前は省略できない
    - `@param`の名前は仮引数の名前に一致するはずなので、自明にLuaの名前規則に従う
    - 一方、`fun`の仮引数の名前は`Name`が許容される
- 返り値の型は名前を付けられる
    - `@return`も`fun`の返り値も`Name`が許容される
    - `fun`の返り値の`...`は曖昧に見える
        - `fun():...:T`は型`T`の可変長返り値
        - `@return ...`と`fun():...`が受容され、意味は`...unknown`になった
        - `fun():...:...`も意味は`...unknown`になった
        - `fun():name:...`の意味は`name:unknown`になった
        - `...`が特殊な名前だとして、型としても扱えるようにすれば良いのかもしれない
        - 型としての`...`は`unknown`になるらしい
        - `Name`が`...`を含まない場合もありそう
            - 意味論のほうではじく
- 返り値の型リストは括弧で囲める
    - `fun ():(T1, T2, T3)`
    - 単一の`(T)`は単に括弧で囲まれているだけと解釈
    - 仮引数のリストと似ているが仮引数は名前が必須
        - CPSみたいなかんじかもしれない
    - `fun ():()`はだめ
- Lua 5.5の名前付き可変長引数は`fun`では書けない
    - 型の定義としては不要なので問題なさそう

```
name ::= Name | '...'
type ::= name | Integer | String | Code |
    tuple_type |
    table_type |
    function_type |
    generics_type |
    type binary_operator type |
    type suffix_operator |
    '(' type ')'
types = type {',' type}

tuple_type ::= '[' types ']'

table_type ::= '{' [fields] '}'
field ::= '[' type ']' ':' type | name ':' type
field_seprator ::= ',' | ';'
fields ::= field {field_separtor field} [field_separator]

function_type ::= 'fun' '(' [params] ')' [':' return_type]
param ::= name | name ':' type
params ::= param {',' param}
return ::= type | name ':' type
returns ::= return {',' return}
return_type = returns | '(' returns ')'

generics_type ::= type '<' types '>'

binary_operator ::= '|'
suffix_operator ::= '?' | '[]'

operator_type ::= Name {'(' type ')'} ':' type
```

### 演算子と優先順位

- 低いほうから高いほうへ

| 演算子              | 結合   |
|---------------------|--------|
| <code>&#x7C;</code> | 左結合 |
| `?`, `[]`           | 後置   |

