# dromozoa

Luaに型付けを行うために、以下の制約を考える。
1. requireを静的に解決する。
2. chunkレベルで解析を行う。
3. coroutineとerrorは使わない。

コンパイル時に
1. 非決定な環境にアクセスするか、
2. チャンク実行中に可変長引数にアクセスするまで、
チャンクを実行する。

初期段階では、この段階で文字列とテーブルをすべて解決してしまうことにする。

## 基本型

* int
* ptr
* i32
* i64
* f32
* f64

デバッグ用に標準出力用の関数を用意する。

## 型アノテーション

```Lua
local i32 = require "dromozoa.annotation.i32"

local function f(a, b, c)
  local v = i32(a) + b + c
  return v
end
```

```
basic: 'i32' | 'i64' | 'f32' | 'f64'
tuple: '(' [type {',' type}] ')'
function: tuple '->' tuple
type: basic | function
```



