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

| アノテーション | 方針 |
|----------------|------|
| `@alias`       | 実装 |
| `@as`          | 検討 |
| `@async`       | 無視 |
| `@cast`        | 検討 |
| `@class`       | 実装 |
| `@deprecated`  | 無視 |
| `@diagnostic`  | 無視 |
| `@enum`        | 検討 |
| `@field`       | 実装 |
| `@generic`     | 実装 |
| `@meta`        | 無視 |
| `@module`      | 実装 |
| `@nodiscard`   | 無視 |
| `@operator`    | 実装 |
| `@overload`    | 実装 |
| `@package`     | 実装 |
| `@param`       | 実装 |
| `@private`     | 実装 |
| `@protected`   | 実装 |
| `@public`      | 実装 |
| `@return`      | 実装 |
| `@see`         | 無視 |
| `@source`      | 無視 |
| `@type`        | 実装 |
| `@vararg`      | 無視 |
| `@version`     | 無視 |

- 継続行の`---|`も検討とする

## 字句解析

- `Name`の範囲が広い
    - Luaのパターンで表現すると`[%a_\x80-\xFF][%w_.*%-\x80-\xFF]*`
- キーワードは文脈依存なので構文解析側で処理する
