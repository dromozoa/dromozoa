# LuaLS


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
```


---

# main


```lua
function main()
  -> integer
```


---

# node

## file


```lua
string
```

## kind


```lua
integer
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