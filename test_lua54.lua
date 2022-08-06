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

local source_filename = ...
local handle = assert(io.open(source_filename, "rb"))
local source = handle:read "*a"
handle:close()

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

local regexp_filename = "test-gen-lua54-regexp.lua"
local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
  regexp.machine.lexer(token_names, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    _"return";

    _"(";
    _")";
    _",";
    _";";

    -- short comment
    _"--" + -_{"\n\r"}*"*";

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
    = _"functioncall"
    ;

  ["{stat}"]
    = _
    + _"{stat}" "stat"
    ;

  retstat
    = _"return" "[explist]" "[;]"
    ;

  ["[retstat]"]
    = _
    + _"retstat"
    ;

  var
    = _"Name"
    ;

  explist
    = _"exp" "{, exp}"
    ;

  ["[explist]"]
    = _
    + _"explist"
    ;

  exp
    = _"Numeral"
    ;

  prefixexp
    = _"var"
    ;

  functioncall
    = _"prefixexp" "args"
    ;

  args
    = _"(" "[explist]" ")"
    ;

  ["{, exp}"]
    = _
    + _"{, exp}" ", exp"
    ;

  [", exp"]
    = _"," "exp"
    ;

  ["[;]"]
    = _
    + _";"
    ;

}))
for _, message in conflictions:ipairs() do
  print(message)
end

local parser_filename = "test-gen-lua54-parser.lua"
local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(grammar, actions))
out:close()

local lua54_regexp = assert(assert(loadfile(regexp_filename))())
local lua54_parser = assert(assert(loadfile(parser_filename))())
local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, lua54_parser())

local function quote(s)
  return [["]] .. string.gsub(s, "[<>&]", { ["<"] = "&lt;", [">"] = "&gt;", ["&"] = "&amp;" }) .. [["]]
end

local function dump(u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  io.write(("  "):rep(n), "<node name=", quote(lua54_parser.symbol_names[u[0]]))
  if verbose then
    if u.i ~= nil then io.write(" i=", quote(u.i)) end
    if u.j ~= nil then io.write(" j=", quote(u.j)) end
    if u.n ~= nil then io.write(" n=", quote(u.n)) end
    if u.c ~= nil then io.write(" c=", quote(u.c)) end
  end
  if u.v ~= nil then io.write(" v=", quote(u.v)) end
  if #u == 0 then
    io.write "/>\n"
  else
    io.write ">\n"
    for _, v in ipairs(u) do
      dump(v, n)
    end
    io.write(("  "):rep(n), "</node>\n")
  end
end

dump(root)
