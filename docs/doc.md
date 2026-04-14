# LuaLS


---

# Node

## file


```lua
string
```

## kind


```lua
string
```

## position


```lua
integer
```

## value


```lua
string
```


---

# export_start


```lua
function export_start(main: fun():0|1)
```


---

# get_arguments


```lua
function get_arguments()
  -> string[]
```


---

# lexer


```lua
function lexer(filename: string, source: string)
  -> Node[]
```


---

# main


```lua
function main()
  -> integer
```


---

# new_token


```lua
function new_token(kind: string, value: string, file: string, position: integer)
  -> Node
```


---

# parse_file


```lua
function parse_file(filename: string)
```


---

# read_file


```lua
function read_file(filename: string)
  -> string
```