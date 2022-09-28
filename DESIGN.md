# dromozoa

## ステージ

* stage1 transpiler

## 命令セット

* Lua, WASM, Javaを参考にする

* 制御構造はWASMを参考にする。
  * `br`系は0,1だけでよいかも。
* `block`
* `loop`
* `if else end`は使う？
* `block-loop`とかあるとよくない？

### 局所変数

```
Lua
レジスタ指定なので存在しない。

WASM
local.get
local.set
local.tee

Java
[t]load
[t]store
```

### 分岐

```
Lua
test

WASM
br_if
if ... else ... end

Java
if*
```

### JMP

```
Lua
jmp

WASM
brでなんとかする

Java
goto
goto_w

```

### LOOP

```
do {
} while (expr...);

LOOP: loop


  expr
  br_if LOOP
end

while (expr) {
}


repeat
  expr
  if
    :
    :
    :
    continue
  else
    break
  end
end

LOOP: loop
  expr
  IF: if
    :
    break => br IF;
    :
    br loop
  else
    //...
  end
end

BLOCK
  LOOP


  END
END

```

### 関数呼び出し

`lua_call`と同じ。

```
function
arg1
 :
argN
call(nargs, nresults)
```

関数の返り値の個数は`#`か`nresults`レジスタ？に入る。これをつかって、
もしくは、スタックトップを保持しておくか、絶対アドレス指定する

* Luaのopcodeのように位置をおぼえておくほうが簡単ではある
* nresults
* top
* abs(1)
* nr(+1)
* top-3
* multi(-1)
* multi呼び出しの場合はnresultsが更新されることが保証される

```
local a, b, c = f(1, 2, f(3, 4, 5))

push f
push 1
push 2
push f
push 3
push 4
push 5
call(4,-1)
call(1,3)
local.set 3 c
local.set 2 b
local.set 1 a
```

```
  (*) In OP_CALL, if (B == 0) then B = top - A. If (C == 0), then
  'top' is set to last_result+1, so next open instruction (OP_CALL,
  OP_RETURN*, OP_SETLIST) may use 'top'.

  (*) In OP_VARARG, if (C == 0) then use actual number of varargs and
  set top (like in OP_CALL with C == 0).
```


