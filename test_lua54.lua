-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
--
-- This file is part of dromozoa.
--
-- dromozoa is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- dromozoa is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local verbose = os.getenv "VERBOSE" == "1"

local dir = assert(...)

local array = require "dromozoa.array"
local regexp = {
  pattern = require "dromozoa.regexp.pattern";
  machine = require "dromozoa.regexp.machine";
  compile = require "dromozoa.regexp.compile";
}
local parser = {
  grammar = require "dromozoa.parser.grammar";
  lalr = require "dromozoa.parser.lalr";
  compile = require "dromozoa.parser.compile";
}

local _ = regexp.pattern

local token_names = array()

local regexp_filename = dir .. "/test_lua54_regexp.lua"
local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
  "local ra";

  long_comment = regexp.machine.guard("freturn()", {
    _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    _(_);
  });

  long_literal_string = regexp.machine.guard("freturn()", {
    _"\n"/"append(0x0A) ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"append(0x0A) ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    _(_)/"append(fc)";
  });

  short_literal_string = regexp.machine.guard("freturn()", {
    _"\\" + _{
      _"a"/"append(0x07)";
      _"f"/"append(0x0C)";
      _"n"/"append(0x0A)";
      _"r"/"append(0x0D)";
      _"t"/"append(0x09)";
      _"v"/"append(0x0B)";
      _"\\"/"append(fc)";
      _"\""/"append(fc)";
      _"\'"/"append(fc)";
      _"\n"/"append(0x0A) ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"append(0x0A) ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
      _"z" + _{
        _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
        _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
        _{" \f\t\v"}*"+"
      }*"*";
      _"x" + _{
        _["09"]/"ra=fc-${<0>}";
        _["af"]/"ra=fc-${<a>}+10";
        _["AF"]/"ra=fc-${<A>}+10";
      } + _{
        _["09"]/"append(ra*16+fc-${<0>})";
        _["af"]/"append(ra*16+fc-${<a>}+10)";
        _["AF"]/"append(ra*16+fc-${<A>}+10)";
      };
      _"u" + _"{"/"ra=0" + _{
        _["09"]/"ra=ra*16+fc-${<0>}";
        _["af"]/"ra=ra*16+fc-${<a>}+10";
        _["AF"]/"ra=ra*16+fc-${<A>}+10";
      }*"+" + _"}"/"fassert(ra<=0x7FFFFFFF,'UTF-8 value too large') append_unicode(ra)";
    };

    (_"\\" + _["09"]/"ra=fc-${<0>}" + _["09"]/"ra=ra*10+fc-${<0>}"*{0,2}) %"fassert(ra<=255,'decimal escape too large') append(ra)";

    _(_)/"append(fc)";
  });

  regexp.machine.lexer(token_names, {
    ----------------------------------------------------------------------------
    -- Spaces
    _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
      _{" \f\t\v"}*"+";
    }*"+";

    ----------------------------------------------------------------------------
    -- Comment

    (_"--" + _"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})") %"fcall($long_comment) push()";

    _"--" + -_{"\n\r"}*"*";

    ----------------------------------------------------------------------------
    -- LiteralString

    LongLiteralString = (_"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})" + _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    }*"?") %"clear() fcall($long_literal_string) push(true)";

    ShortLiteralString = _{"\'\""}/"guard_clear(fc)" %"clear() fcall($short_literal_string) push(true)";

    ----------------------------------------------------------------------------
    -- Numeral

    -- 8進数は存在しないので、leading zerosも許容される。
    DecimalIntegerNumeral = _["09"]*"+";

    -- C言語のリテラルのdecimal-floating-constantに類似しているが、以下の点で異
    -- なる。
    -- 1. 小数点も指数部もない場合はDecimalIntegerNumeralがマッチするので除外し
    --    ない。
    -- 2. 接尾辞は持たない。
    DecimalFloatingNumeral = _{
      _["09"]*"*" + _"." + _["09"]*"+";
      _["09"]*"+" + _"."*"?";
    } + (_{"eE"} + _{"+-"}*"?" + _["09"]*"+")*"?";

    HexadecimalIntegerNumeral = _"0" + _{"xX"} + _["09AFaf"]*"+";

    -- C言語のリテラルのhexadecimal-floating-constantに類似しているが、以下の点
    -- で異なる。
    -- 1. 指数部を省略できる。C言語のリテラルでは指数を省略できないが、strtodで
    --    は省略できる。
    -- 2. 小数点も指数部もない場合はHexadecimalIntegerNumeralがマッチするので除
    --    外しない。
    -- 3. 接尾辞は持たない。
    HexadecimalFloatingNumeral = _"0" + _{"xX"} + _{
      _["09AFaf"]*"*" + _"." + _["09AFaf"]*"+";
      _["09AFaf"]*"+" + _"."*"?";
    } + (_{"pP"} + _{"+-"}*"?" + _["09"]*"+")*"?"
    ;

    ----------------------------------------------------------------------------

    _"break";
    _"do";
    _"end";
    _"for";
    _"function";
    _"goto";
    _"in";
    _"local";
    _"repeat";
    _"return";
    _"until";
    _"while";

    _"(";
    _")";
    _",";
    _".";
    _":";
    _"::";
    _";";
    _"<";
    _"=";
    _">";
    _"[";
    _"]";
    _"{";
    _"}";

    ----------------------------------------------------------------------------
    -- Name

    Name = _["AZaz_"] + _["09AZaz_"]*"*";
  });
})
out:close()

local _ = parser.grammar.body
local left = parser.grammar.left
local right = parser.grammar.right

local grammar, actions, conflictions = parser.lalr(parser.grammar(token_names, {
  chunk = _"block";

  block = _"{stat}" "[retstat]";

  stat
    = ";"
    + _"varlist" "=" "explist"
    + _"functioncall"
    + _"label"
    + _"break"
    + _"goto" "Name"
    + _"do" "block" "end"
    + _"while" "exp" "do" "block" "end"
    + _"repeat" "block" "until" "exp"
    + _"for" "Name" "=" "exp" "," "exp" "[, exp]" "do" "block" "end"
    + _"for" "namelist" "in" "explist" "do" "block" "end"
    + _"function" "funcname" "funcbody"
    + _"local" "function" "Name" "funcbody"
    + _"local" "attnamelist" "[= explist]"
    ;

  ["{stat}"]
    = _
    + _"{stat}" "stat" %"$$=$1 append($2)"
    ;

  ------------------------------------------------------------------------------

  attnamelist
    = _"Name" "attrib" "{, Name attrib}" -- $$=$0 append($1) $1.attrib=$2.v append_unpack($3)
    ;

  -- attrib = _"[< Name >]";

  -- ["[< Name >]"]
  --   = _
  --   + _"<" "Name" ">"
  --   ;

  attrib
    = _
    + _"<" "Name" ">" %"$$=$0 append($2)" -- $$=$0 $$.v=$2.v
    ;

  ["{, Name attrib}"]
    = _
    + _"{, Name attrib}" "," "Name" "attrib" %"$$=$1 append($3, $4)" -- $$=$1 append($3) $3.attrib=$4.v
    ;

  ------------------------------------------------------------------------------

  retstat
    = _"return" "[explist]"
    + _"return" "[explist]" ";" %"$$=$0 append($1,$2)"
    ;

  ["[retstat]"]
    = _
    + _"retstat"
    ;

  ------------------------------------------------------------------------------

  label
    = _"::" "Name" "::"
    ;

  ------------------------------------------------------------------------------

  funcname
    = _"Name" "{. Name}" "[: Name]"
    ;

  ["{. Name}"]
    = _
    + _"{. Name}" "." "Name"
    ;

  ["[: Name]"]
    = _
    + _":" "Name"
    ;

  ------------------------------------------------------------------------------

  varlist
    = _"var" "{, var}" %"$$=$0 append($1) append_unpack($2)"
    ;

  ["{, var}"]
    = _                    %"create($varlist)"
    + _"{, var}" "," "var" %"$$=$1 append($3)"
    ;

  var
    = _"Name"
    + _"prefixexp" "[" "exp" "]"
    + _"prefixexp" "." "Name"
    ;

  namelist = _"Name" "{, Name}";

  ["{, Name}"]
    = _
    + _"{, Name}" "," "Name"
    ;

  explist
    = _"exp"
    + _"explist" "," "exp" %"$$=$1 append($3)"
    ;

  ["[explist]"]
    = _          %"create($explist)"
    + _"explist" %"$$=$1"
    ;

  ["[= explist]"]
    = _
    + _"=" "explist"
    ;

  exp
    = _"Numeral"
    + _"LiteralString"
    + _"prefixexp"
    ;

  ["[, exp]"]
    = _
    + _"," "exp"
    ;

  prefixexp
    = _"var"
--    + _"functioncall"
    + _"(" "exp" ")"
    ;

  functioncall
    = _"prefixexp" "args"
    + _"prefixexp" ":" "Name" "args"
--    + _"functioncall" "args"
    + _"functioncall" ":" "Name" "args"
    ;

  args
    = _"(" "[explist]" ")" %"$$=$0 append($2)"
    + _"tableconstructor"
    ;

  funcbody = _"(" "parlist" ")" "block" "end";

  parlist
    = _"namelist" -- ...
    ;

  tableconstructor
    = _"{" "[fieldlist]" "}"
    ;

  ["[fieldlist]"]
    = _            %"create($fieldlist)"
    + _"fieldlist" %"$$=$1"
    ;

  fieldlist
    = _"field" "{fieldsep field}" "[fieldsep]" %"$$=$0 append($1) append_unpack($2)"
    ;

  ["{fieldsep field}"]
    = _
    + _"{fieldsep field}" "fieldsep" "field" %"$$=$1 append($3)"
    ;

  field
    = _"[" "exp" "]" "=" "exp"
    + _"Name" "=" "exp"
    + _"exp"
    ;

  fieldsep
    = _","
    + _";"
    ;

  ["[fieldsep]"]
    = _
    + _"fieldsep"
    ;

  LiteralString
    = _"LongLiteralString"
    + _"ShortLiteralString"
    ;

  Numeral
    = _"DecimalIntegerNumeral"
    + _"DecimalFloatingNumeral"
    + _"HexadecimalIntegerNumeral"
    + _"HexadecimalFloatingNumeral"
    ;

}))
for _, message in conflictions:ipairs() do
  print(message)
end

local parser_filename = dir .. "/test_lua54_parser.lua"
local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(grammar, actions))
out:close()

local lua54_regexp = assert(assert(loadfile(regexp_filename))())
local lua54_parser = assert(assert(loadfile(parser_filename))())

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local function dump(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<node")
  if u[0] ~= nil then out:write(" name=", quote(lua54_parser.symbol_names[u[0]])) end
  if verbose then
    if u.i ~= nil then out:write(" i=", quote(u.i)) end
    if u.j ~= nil then out:write(" j=", quote(u.j)) end
    if u.n ~= nil then out:write(" n=", quote(u.n)) end
    if u.c ~= nil then out:write(" c=", quote(u.c)) end
  end
  if u.v ~= nil then out:write(" v=", quote(u.v)) end
  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump(out, v, n)
    end
    out:write(("  "):rep(n), "</node>\n")
  end
end

for i = 2, #arg do
  local source_filename = assert(arg[i])
  local result_basename = assert(source_filename:match "([^/]+)%.lua$")
  result_basename = dir .. "/" .. result_basename

  local handle = assert(io.open(source_filename))
  local source = handle:read "*a"
  handle:close()

  local out = assert(io.open(result_basename .. "_list.xml", "w"))
  out:write "<nodes>\n"

  local parse = lua54_parser()
  local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, function (token)
    dump(out, token)
    return parse(token)
  end)

  out:write "</nodes>\n"
  out:close()

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump(out, root)
  out:close()
end
