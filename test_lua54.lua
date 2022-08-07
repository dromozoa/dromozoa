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
  long_comment = regexp.machine.guard("freturn()", {
    _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    _"]";
    _(_);
  });

  long_literal_string = regexp.machine.guard("freturn()", {
    _"\n"/"ln=ln+1 lp=fp append(0x0A)" + _"\r"/"lp=fp"*"?";
    _"\r"/"ln=ln+1 lp=fp append(0x0A)" + _"\n"/"lp=fp"*"?";
    _"]"/"append(fc)";
    _(_)/"append(fc)";
  });

  regexp.machine.lexer(token_names, {
    _{" \t\f\v"}*"+";
    _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";

    -- long comment
    (_"--" + _"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})") %"fcall($long_comment) push()";

    -- short comment
    _"--" + -_{"\n\r"}*"*";

    LongLiteralString = (_"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})" + _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    }*"?") %"clear() fcall($long_literal_string) push(true)";

    _"local";
    _"return";
    _"break";
    _"goto";
    _"do";
    _"end";

    _"(";
    _")";
    _",";
    _";";
    _"=";
    _"[";
    _"]";
    _".";
    _":";
    _"{";
    _"}";
    _"<";
    _">";
    _"::";

    Name
      = _["AZaz_"] + _["09AZaz_"]*"*"
      ;

    Numeral
      = _{
          _["09"]*"+";
        }
      ;
  });
})
out:close()

local _ = parser.grammar.body
local left = parser.grammar.left
local right = parser.grammar.right

local grammar, actions, conflictions = parser.lalr(parser.grammar(token_names, {
  chunk
    = _"block"
    ;

  block
    = _"{stat}" "[retstat]"
    ;

  stat
    = _"varlist" "=" "explist"
    + _"functioncall"
    + _"label"
    + _"break"
    + _"goto" "Name"
    + _"do" "block" "end"
    + _"local" "attnamelist" "[= explist]"
    ;

  ["{stat}"]
    = _
    + _"{stat}" "stat" %"$$=$1 append($2)"
    ;

  ["[= explist]"]
    = _
    + _"=" "explist"
    ;

  attnamelist
    = _"Name" "attrib" "{, Name attrib}"
    ;

  attrib
    = _"[< Name >]"
    ;

  ["[< Name >]"]
    = _
    + _"<" "Name" ">"
    ;

  ["{, Name attrib}"]
    = _
    + _"{, Name attrib}" "," "Name" "attrib"
    ;

  retstat
    = _"return" "[explist]"
    + _"return" "[explist]" ";" %"$$=$0 append($1,$2)"
    ;

  ["[retstat]"]
    = _
    + _"retstat"
    ;

  label
    = _"::" "label" "::"
    ;

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

  explist
    = _"exp"
    + _"explist" "," "exp" %"$$=$1 append($3)"
    ;

  ["[explist]"]
    = _          %"create($explist)"
    + _"explist" %"$$=$1"
    ;

  exp
    = _"Numeral"
    + _"LongLiteralString"
--    + _"prefixexp"
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
