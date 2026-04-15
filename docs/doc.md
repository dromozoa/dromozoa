# Kind


---

# LuaLS


---

# Node

## file


```lua
string
```

## kind


```lua
Kind
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

# integer_min


```lua
function integer_min(a: integer, b: integer)
  -> integer
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
function new_token(kind: Kind, value: string, file: string, position: integer)
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


---

# string_byte


```lua
function string_byte(s: string, i: integer)
  -> integer
```


---

# string_char


```lua
function string_char(list: integer[])
  -> string
```


---

# string_compare


```lua
function string_compare(a: string, b: string)
  -> integer
```


---

# string_find


```lua
function string_find(s: string, search: string, position: integer)
  -> integer
```


---

# string_len


```lua
function string_len(s: string)
  -> integer
```


---

# string_starts_with


```lua
function string_starts_with(s: string, search: string, position: integer)
  -> boolean
```


---

# string_sub


```lua
function string_sub(s: string, i: integer, j: integer)
  -> string
```