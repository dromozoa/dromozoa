# LuaLS


---

# dromozoa.annotation_parser

## led_left


```lua
(method) dromozoa.annotation_parser:led_left(u: any, x: any, rbp: any)
```

## led_suffix


```lua
(method) dromozoa.annotation_parser:led_suffix(u: any, x: any)
```

## lexer


```lua
dromozoa.token_stream
```

## new


```lua
function dromozoa.annotation_parser.new(lexer: dromozoa.token_stream)
  -> dromozoa.annotation_parser
```

## nud_token


```lua
(method) dromozoa.annotation_parser:nud_token(x: any)
```

## parse_expression


```lua
(method) dromozoa.annotation_parser:parse_expression(rbp: any)
  -> dromozoa.node
```

## peek


```lua
(method) dromozoa.annotation_parser:peek()
  -> dromozoa.token
```

## read


```lua
(method) dromozoa.annotation_parser:read()
  -> dromozoa.token
```

## unread


```lua
(method) dromozoa.annotation_parser:unread()
```


---

# dromozoa.annotation_parser.led


---

# dromozoa.annotation_parser.led_function


---

# dromozoa.annotation_parser.nud


---

# dromozoa.lua_parser

## led_call


```lua
(method) dromozoa.lua_parser:led_call(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_index


```lua
(method) dromozoa.lua_parser:led_index(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_left


```lua
(method) dromozoa.lua_parser:led_left(u: dromozoa.node, x: dromozoa.token, rbp: integer)
  -> dromozoa.node
```

## led_member


```lua
(method) dromozoa.lua_parser:led_member(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## led_right


```lua
(method) dromozoa.lua_parser:led_right(u: dromozoa.node, x: dromozoa.token, rbp: integer)
  -> dromozoa.node
```

## led_self


```lua
(method) dromozoa.lua_parser:led_self(u: dromozoa.node, x: dromozoa.token)
  -> dromozoa.node
```

## lexer


```lua
dromozoa.token_stream
```

## new


```lua
function dromozoa.lua_parser.new(lexer: dromozoa.token_stream)
  -> dromozoa.lua_parser
```

## nud_function


```lua
(method) dromozoa.lua_parser:nud_function(x: dromozoa.token)
  -> dromozoa.node
```

## nud_group


```lua
(method) dromozoa.lua_parser:nud_group(x: dromozoa.token)
  -> dromozoa.node
```

## nud_prefix


```lua
(method) dromozoa.lua_parser:nud_prefix(x: dromozoa.token)
  -> dromozoa.node
```

## nud_table


```lua
(method) dromozoa.lua_parser:nud_table(x: dromozoa.token)
  -> dromozoa.node
```

## nud_token


```lua
(method) dromozoa.lua_parser:nud_token(x: dromozoa.token)
  -> dromozoa.node
```

## parse


```lua
(method) dromozoa.lua_parser:parse()
  -> dromozoa.node
```

## parse_args


```lua
(method) dromozoa.lua_parser:parse_args(x: dromozoa.token)
  -> dromozoa.node
```

## parse_assignment


```lua
(method) dromozoa.lua_parser:parse_assignment(u: dromozoa.node)
  -> dromozoa.node
```

## parse_block


```lua
(method) dromozoa.lua_parser:parse_block(kind?: string)
  -> dromozoa.node
```

## parse_declaration


```lua
(method) dromozoa.lua_parser:parse_declaration(kind: "global"|"local")
  -> dromozoa.node
```

```lua
kind:
    | "global"
    | "local"
```

## parse_exp


```lua
(method) dromozoa.lua_parser:parse_exp(rbp?: integer)
  -> dromozoa.node
```

## parse_explist


```lua
(method) dromozoa.lua_parser:parse_explist()
  -> dromozoa.node
```

## parse_field


```lua
(method) dromozoa.lua_parser:parse_field(x: dromozoa.token)
  -> dromozoa.node
```

## parse_funcbody


```lua
(method) dromozoa.lua_parser:parse_funcbody()
  -> dromozoa.node
```

## parse_funcname


```lua
(method) dromozoa.lua_parser:parse_funcname()
  -> dromozoa.node
```

## parse_generic_for


```lua
(method) dromozoa.lua_parser:parse_generic_for(x: dromozoa.token, y: dromozoa.token)
  -> dromozoa.node
```

## parse_if


```lua
(method) dromozoa.lua_parser:parse_if(x: dromozoa.token)
  -> dromozoa.node
```

## parse_led


```lua
(method) dromozoa.lua_parser:parse_led(u: dromozoa.node, rbp: integer, led_table: table<string, { lbp: integer, fn: fun(parser: dromozoa.lua_parser, u: dromozoa.node, x: dromozoa.token, rbp: integer):dromozoa.node }>)
  -> dromozoa.node
```

## parse_nud


```lua
(method) dromozoa.lua_parser:parse_nud(nud_table: table<string, fun(parser: dromozoa.lua_parser, x: dromozoa.token):dromozoa.node>)
  -> (dromozoa.node)?
  2. string?
```

## parse_numeric_for


```lua
(method) dromozoa.lua_parser:parse_numeric_for(x: dromozoa.token, y: dromozoa.token)
  -> dromozoa.node
```

## parse_prefixexp


```lua
(method) dromozoa.lua_parser:parse_prefixexp(rbp?: integer)
  -> dromozoa.node
```

## parse_retstat


```lua
(method) dromozoa.lua_parser:parse_retstat()
  -> dromozoa.node
```

## parse_stat


```lua
(method) dromozoa.lua_parser:parse_stat()
  -> dromozoa.node
```

## parse_tableconstructor


```lua
(method) dromozoa.lua_parser:parse_tableconstructor(x: dromozoa.token)
  -> dromozoa.node
```

## peek


```lua
(method) dromozoa.lua_parser:peek()
  -> dromozoa.token
```

## read


```lua
(method) dromozoa.lua_parser:read()
  -> dromozoa.token
```

## unread


```lua
(method) dromozoa.lua_parser:unread()
```


---

# dromozoa.lua_parser.led


---

# dromozoa.lua_parser.led_function


---

# dromozoa.lua_parser.nud


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

## _2


```lua
string?
```

## escape


```lua
function dromozoa.matcher.escape(source: string)
  -> string
```

## is_at_end


```lua
(method) dromozoa.matcher:is_at_end()
  -> boolean
```

## is_at_start


```lua
(method) dromozoa.matcher:is_at_start()
  -> boolean
```

## match


```lua
(method) dromozoa.matcher:match(pattern: string)
  -> boolean
```

## match_long_string


```lua
(method) dromozoa.matcher:match_long_string()
  -> boolean
```

## match_short_string


```lua
(method) dromozoa.matcher:match_short_string()
  -> boolean
```

## new


```lua
function dromozoa.matcher.new(source: string, srcloc: dromozoa.source_location)
  -> dromozoa.matcher
```

## offset


```lua
integer
```

## source


```lua
string
```

## srcloc


```lua
dromozoa.source_location
```

## substring


```lua
(method) dromozoa.matcher:substring(srcloc: dromozoa.source_location)
  -> string
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

# dromozoa.token_stream

## index


```lua
integer
```

## lex


```lua
fun(matcher: dromozoa.matcher):dromozoa.token
```

## matcher


```lua
dromozoa.matcher
```

## new


```lua
function dromozoa.token_stream.new(lex: fun(matcher: dromozoa.matcher):dromozoa.token, matcher: dromozoa.matcher)
  -> dromozoa.token_stream
```

## peek


```lua
(method) dromozoa.token_stream:peek()
  -> dromozoa.token
```

## read


```lua
(method) dromozoa.token_stream:read()
  -> dromozoa.token
```

## tokens


```lua
dromozoa.token[]
```

## unread


```lua
(method) dromozoa.token_stream:unread()
```


---

# dromozoa.token_stream.lex


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


---

# parser.object

## as


```lua
(parser.object)?
```

## async


```lua
boolean?
```

## bindComments


```lua
parser.object[]?
```

## calls


```lua
parser.object[]?
```

## docAttr


```lua
(parser.object)?
```

## generic


```lua
(parser.object)?
```

## generics


```lua
parser.object[]?
```

## literal


```lua
boolean
```

## module


```lua
string?
```

## names


```lua
parser.object[]?
```

## operators


```lua
parser.object[]?
```

## originalComment


```lua
parser.object
```

## path


```lua
string?
```

## pattern


```lua
string?
```

## signs


```lua
parser.object[]
```

## touch


```lua
integer?
```

## versions


```lua
table[]?
```

## visible


```lua
('package'|'private'|'protected'|'public')?
```


---

# parser.visibleType