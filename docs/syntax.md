# 文法

## Lua 5.5のEBNF

https://www.lua.org/manual/5.5/manual.html#9

```
chunk ::= block

block ::= {stat} [retstat]

stat ::= ';' |
    varlist '=' explist |
    functioncall |
    label |
    'break' |
    'goto' Name |
    'do' block 'end' |
    'while' exp 'do' block 'end' |
    'repeat' block 'until' exp |
    'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end' |
    'for' Name '=' exp ',' exp [',' exp] 'do' block 'end' |
    'for' namelist 'in' explist 'do' block 'end' |
    'function' funcname funcbody |
    'local' 'function' Name funcbody |
    'global' 'function' Name funcbody |
    'local' attnamelist ['=' explist] |
    'global' attnamelist |
    'global' [attrib] '*'

attnamelist ::= [attrib] Name [attrib] {',' Name [attrib]}

attrib ::= '<' Name '>'

retstat ::= 'return' [explist] [';']

label ::= '::' Name '::'

funcname ::= Name {'.' Name} [':' Name]

varlist ::= var {',' var}

var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name

namelist ::= Name {',' Name}

explist ::= exp {',' exp}

exp ::= 'nil' | 'false' | 'true' | Numeral | LiteralString | '...' | functiondef |
    prefixexp | tableconstructor | exp binop exp | unop exp

prefixexp ::= var | functioncall | '(' exp ')'

functioncall ::= prefixexp args | prefixexp ':' Name args

args ::= '(' [explist] ')' | tableconstructor | LiteralString

functiondef ::= 'function' funcbody

funcbody ::= '(' [parlist] ')' block 'end'

parlist ::= namelist [',' varargparam] | varargparam

varargparam ::= '...' [Name]

tableconstructor ::= '{' [fieldlist] '}'

fieldlist ::= field {fieldsep field} [fieldsep]

field ::= '[' exp ']' '=' exp | Name '=' exp | exp

fieldsep ::= ',' | ';'

binop ::= '+' | '-' | '*' | '/' | '//' | '^' | '%' |
    '&' | '~' | '|' | '>>' | '<<' | '..' |
    '<' | '<=' | '>' | '>=' | '==' | '~=' |
    'and' | 'or'

unop ::= '-' | 'not' | '#' | '~'
```

## Lua 5.5の優先順位

```
or
and
<     >     <=    >=    ~=    ==
|
~
&
<<    >>
..
+     -
*     /     //    %
unary operators (not   #     -     ~)
^
```

- 文法規則`var`, `prefixexp`, `functioncall`を演算子で表現する
- これらの演算子は`^`よりも高い優先順位を持ち、左結合である
- 文法的に不正な結合は行わないように注意する

```
a()   function call
aS    function call (LiteralString)
a{}   function call (tableconstructor)
a[]   subscript
a.b   field access
```
