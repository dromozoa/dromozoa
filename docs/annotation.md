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
| @alias         |      |
| @as            |      |
| @async         |      |
| @cast          |      |
| @class         |      |
| @deprecated    |      |
| @diagnostic    |      |
| @enum          |      |
| @field         |      |
| @generic       |      |
| @meta          |      |
| @module        |      |
| @nodiscard     |      |
| @operator      |      |
| @overload      |      |
| @package       |      |
| @param         |      |
| @private       |      |
| @protected     |      |
| @public        |      |
| @return        |      |
| @see           |      |
| @source        |      |
| @type          |      |
| @vararg        |      |
| @version       |      |

## 字句解析

- `Name`の範囲が広い
    - Luaのパターンで表現すると`[%a_\x80-\xFF][%w_.*%-\x80-\xFF]*`
- キーワードは文脈依存なので構文解析側で処理する
