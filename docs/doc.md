# LuaLS


---

# dromozoa.led


---

# dromozoa.led_function


---

# dromozoa.lexer

## _0


```lua
string
```

## _1


```lua
string?
```

## _2


```lua
string?
```

## eof


```lua
(method) dromozoa.lexer:eof()
  -> boolean
```

## lex


```lua
(method) dromozoa.lexer:lex(source: string, filename: string)
  -> dromozoa.token[]
```

## lex_punctuator


```lua
(method) dromozoa.lexer:lex_punctuator()
  -> boolean
```

## match


```lua
(method) dromozoa.lexer:match(pattern: string)
  -> boolean
```

## new


```lua
function dromozoa.lexer.new()
  -> dromozoa.lexer
```

## source


```lua
string?
```

## srcloc


```lua
(dromozoa.source_location)?
```


---

# dromozoa.node

## append


```lua
(method) dromozoa.node:append(node: dromozoa.node)
  -> dromozoa.node
```

## attribute


```lua
(dromozoa.token)?
```

## category


```lua
"auxiliary"|"block"|"expression"|"statement"
```

## check


```lua
(method) dromozoa.node:check(...kinds: string)
  -> boolean
```

## extend


```lua
(method) dromozoa.node:extend(nodes: dromozoa.node[])
  -> dromozoa.node
```

## kind


```lua
string
```

## new


```lua
function dromozoa.node.new(category: "auxiliary"|"block"|"expression"|"statement", kind: string, token?: dromozoa.token)
  -> dromozoa.node
```

```lua
category:
    | "block"
    | "statement"
    | "expression"
    | "auxiliary"
```

## nodes


```lua
dromozoa.node[]
```

## require


```lua
(method) dromozoa.node:require(...string)
  -> dromozoa.node
```

## srcloc


```lua
(method) dromozoa.node:srcloc()
  -> (dromozoa.source_location)?
```

## token


```lua
(dromozoa.token)?
```


---

# dromozoa.node.category


---

# dromozoa.nud


---

# dromozoa.parser

## index


```lua
integer
```

## led_call


```lua
(method) dromozoa.parser:led_call(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_index


```lua
(method) dromozoa.parser:led_index(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_left


```lua
(method) dromozoa.parser:led_left(u: dromozoa.node, x: dromozoa.token, rbp: integer)
  -> dromozoa.node
```

## led_member


```lua
(method) dromozoa.parser:led_member(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_right


```lua
(method) dromozoa.parser:led_right(u: dromozoa.node, x: dromozoa.token, rbp: integer)
  -> dromozoa.node
```

## led_self


```lua
(method) dromozoa.parser:led_self(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## new


```lua
function dromozoa.parser.new()
  -> dromozoa.parser
```

## nud_function


```lua
(method) dromozoa.parser:nud_function(x: dromozoa.token)
  -> dromozoa.node
```

## nud_group


```lua
(method) dromozoa.parser:nud_group(x: dromozoa.token)
  -> dromozoa.node
```

## nud_prefix


```lua
(method) dromozoa.parser:nud_prefix(x: dromozoa.token)
  -> dromozoa.node
```

## nud_table


```lua
(method) dromozoa.parser:nud_table(x: dromozoa.token)
  -> dromozoa.node
```

## nud_token


```lua
(method) dromozoa.parser:nud_token(x: dromozoa.token)
  -> dromozoa.node
```

## parse


```lua
(method) dromozoa.parser:parse(tokens: dromozoa.token[])
  -> dromozoa.node
```

## parse_args


```lua
(method) dromozoa.parser:parse_args(x: dromozoa.token)
  -> dromozoa.node
```

## parse_assignment


```lua
(method) dromozoa.parser:parse_assignment(u: dromozoa.node)
  -> dromozoa.node
```

## parse_block


```lua
(method) dromozoa.parser:parse_block(kind?: string)
  -> dromozoa.node
```

## parse_declaration


```lua
(method) dromozoa.parser:parse_declaration(kind: "global"|"local")
  -> dromozoa.node
```

```lua
kind:
    | "global"
    | "local"
```

## parse_exp


```lua
(method) dromozoa.parser:parse_exp(rbp?: integer)
  -> dromozoa.node
```

## parse_explist


```lua
(method) dromozoa.parser:parse_explist()
  -> dromozoa.node
```

## parse_field


```lua
(method) dromozoa.parser:parse_field(x: dromozoa.token)
  -> dromozoa.node
```

## parse_funcbody


```lua
(method) dromozoa.parser:parse_funcbody()
  -> dromozoa.node
```

## parse_funcname


```lua
(method) dromozoa.parser:parse_funcname()
  -> dromozoa.node
```

## parse_generic_for


```lua
(method) dromozoa.parser:parse_generic_for(x: dromozoa.token, y: dromozoa.token)
  -> dromozoa.node
```

## parse_if


```lua
(method) dromozoa.parser:parse_if(x: dromozoa.token)
  -> dromozoa.node
```

## parse_led


```lua
(method) dromozoa.parser:parse_led(u: dromozoa.node, rbp: integer, led_table: table<string, { lbp: integer, fn: fun(parser: dromozoa.parser, u: dromozoa.node, x: dromozoa.token, rbp: integer):dromozoa.node }>)
  -> dromozoa.node
```

## parse_nud


```lua
(method) dromozoa.parser:parse_nud(nud_table: table<string, fun(parser: dromozoa.parser, x: dromozoa.token):dromozoa.node>)
  -> (dromozoa.node)?
  2. string?
```

## parse_numeric_for


```lua
(method) dromozoa.parser:parse_numeric_for(x: dromozoa.token, y: dromozoa.token)
  -> dromozoa.node
```

## parse_prefixexp


```lua
(method) dromozoa.parser:parse_prefixexp(rbp?: integer)
  -> dromozoa.node
```

## parse_retstat


```lua
(method) dromozoa.parser:parse_retstat()
  -> dromozoa.node
```

## parse_stat


```lua
(method) dromozoa.parser:parse_stat()
  -> dromozoa.node
```

## parse_tableconstructor


```lua
(method) dromozoa.parser:parse_tableconstructor(x: dromozoa.token)
  -> dromozoa.node
```

## peek


```lua
(method) dromozoa.parser:peek()
  -> dromozoa.token
```

## read


```lua
(method) dromozoa.parser:read()
  -> dromozoa.token
```

## tokens


```lua
dromozoa.token[]
```

## unread


```lua
(method) dromozoa.parser:unread()
  -> dromozoa.token
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
function dromozoa.source_location.to_string(self?: dromozoa.source_location)
  -> string
```

## update


```lua
(method) dromozoa.source_location:update(text: string)
```


---

# dromozoa.token

## check


```lua
(method) dromozoa.token:check(...kinds: string)
  -> boolean
```

## kind


```lua
string
```

## new


```lua
function dromozoa.token.new(kind: string, subkind?: string, text: string, value: string|number, srcloc: dromozoa.source_location)
  -> dromozoa.token
```

## new_auxiliary_node


```lua
(method) dromozoa.token:new_auxiliary_node(kind?: string)
  -> dromozoa.node
```

## new_expression_node


```lua
(method) dromozoa.token:new_expression_node(kind?: string)
  -> dromozoa.node
```

## new_statement_node


```lua
(method) dromozoa.token:new_statement_node(kind?: string)
  -> dromozoa.node
```

## require


```lua
(method) dromozoa.token:require(...string)
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