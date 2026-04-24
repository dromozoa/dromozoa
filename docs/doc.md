# LuaLS


---

# dromozoa.lua_lexer

## lex


```lua
function dromozoa.lua_lexer.lex(source: string, filename: string)
  -> dromozoa.token[]
```


---

# dromozoa.lua_parser

## new


```lua
function dromozoa.lua_parser.new()
  -> dromozoa.lua_parser
```

## parse


```lua
(method) dromozoa.lua_parser:parse()
```


---

# dromozoa.matcher

## _0


```lua
string?
```

## _1


```lua
string?
```

## eof


```lua
(method) dromozoa.matcher:eof()
  -> boolean
```

## match


```lua
(method) dromozoa.matcher:match(pattern: string)
  -> boolean
```

## new


```lua
function dromozoa.matcher.new(source: string, filename: string)
  -> dromozoa.matcher
```

## source


```lua
string
```

## srcloc


```lua
dromozoa.source_location
```


---

# dromozoa.node

## new


```lua
function dromozoa.node.new()
  -> dromozoa.node
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

## to_string


```lua
(method) dromozoa.source_location:to_string()
  -> string
```

## update


```lua
(method) dromozoa.source_location:update(text: string)
```


---

# dromozoa.token

## kind


```lua
string
```

## new


```lua
function dromozoa.token.new(kind: string, subkind?: string, text: string, value: string|number, srcloc: dromozoa.source_location)
  -> dromozoa.token
```

## srcloc


```lua
dromozoa.source_location
```

## subkind


```lua
string?
```

## text


```lua
string
```

## value


```lua
string|number
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