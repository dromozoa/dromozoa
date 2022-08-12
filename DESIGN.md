# dromozoa

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

