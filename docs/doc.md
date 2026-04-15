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

# char_class


---

# char_class_new


```lua
function char_class_new()
  -> integer[]
```


---

# char_class_set


```lua
function char_class_set(char_class: integer[], byte: integer)
```


---

# char_class_test


```lua
function char_class_test(char_class: integer[], byte: integer)
  -> boolean
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

# pattern

## char_class


```lua
integer[]?
```

## literal


```lua
string?
```

## m


```lua
integer
```

## n


```lua
integer
```


---

# pattern_negate


```lua
function pattern_negate(char_class: integer[])
  -> integer[]
```


---

# pattern_new


```lua
function pattern_new(char_class?: integer[], literal?: string, m: integer, n: integer)
  -> pattern
```


---

# pattern_optional


```lua
function pattern_optional(char_class: integer[])
  -> pattern
```


---

# pattern_range


```lua
function pattern_range(char_class: integer[], range: string)
  -> integer[]
```


---

# pattern_repeat


```lua
function pattern_repeat(char_class: integer[], m: integer, n: integer)
  -> pattern
```


---

# pattern_set


```lua
function pattern_set(char_class: integer[], set: string)
  -> integer[]
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