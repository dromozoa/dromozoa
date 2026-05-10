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

- `Integer`
    - 十進整数の即値
    - Luaのパターン: `%-?%d+`
    - 使用例:
        - `0|1|string`
        - `{ [1]: boolean, [2]: string? }`
- `String`
    - Luaの文字列リテラルの規則に従う
- `Name`の範囲が広い
    - Luaのパターン: Integerの検査前に1のマッチを試す
        1. `%d+%.[%w_.*%-\x80-\xFF]*`
        2. `[%w_\x80-\xFF][%w_.*%-\x80-\xFF]*`
- `Code`
    - Luaのパターン: `` `[^`]*` ``

## 構文解析

```
types ::= type (',' type)*
type ::= Name |
    type suffix_op
```

### 演算子と優先順位

| 演算子    | 結合   |
|-----------|--------|
| `|`       | 左結合 |
| `?`, `[]` | 後置   |

