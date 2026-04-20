# LuaLS


---

# dromozoa.lua_lexer

## file


```lua
string?
```

## from_file


```lua
function dromozoa.lua_lexer.from_file(file: string)
  -> dromozoa.lua_lexer
```

## from_source


```lua
function dromozoa.lua_lexer.from_source(source: string, file?: string)
  -> dromozoa.lua_lexer
```

## lex


```lua
(method) dromozoa.lua_lexer:lex()
  -> dromozoa.token[]
```

## source


```lua
string
```


---

# dromozoa.source_location

## clone


```lua
(method) dromozoa.source_location:clone()
  -> dromozoa.source_location
```

## column


```lua
integer
```

## filename


```lua
string
```

## line


```lua
integer
```

## new


```lua
function dromozoa.source_location.new(filename: string)
  -> dromozoa.source_location
```

## position


```lua
integer
```

## update


```lua
(method) dromozoa.source_location:update(source: string)
  -> dromozoa.source_location
```


---

# dromozoa.token

## kind


```lua
string
```

## new


```lua
function dromozoa.token.new(kind: string, value: string|integer, srcloc: dromozoa.source_location)
  -> dromozoa.token
```

## srcloc


```lua
dromozoa.source_location
```

## value


```lua
string|integer
```


---

# dromozoa.util

## clone


```lua
function dromozoa.util.clone(source: table)
  -> table
```

## normalize_eol


```lua
function dromozoa.util.normalize_eol(source: string)
  -> string
```

## read_file


```lua
function dromozoa.util.read_file(filename: string)
  -> string
```