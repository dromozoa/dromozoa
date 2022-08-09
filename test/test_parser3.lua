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

local regexp_filename = "test-gen-parser3-regexp.lua"
local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
  regexp.machine.lexer(token_names, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    _"[";
    _"]";
    _"{";
    _"}";
    _"(";
    _")";
    _",";
    _";";

    _"if";
    _"then";
    _"else";
    _"elseif";
    _"end";

    item = _["AZaz"]*"+";
  });
})
out:close()

local _ = parser.grammar.body
local left = parser.grammar.left
local right = parser.grammar.right

local g, a, c = parser.lalr(parser.grammar(token_names, {
  main
    = _"exp" ";" %[[
        $$=$0 append($1)
      ]]
    + _"if_stmt"
    ;

  exp
    = _"[" "rlist" "]" %[[
        $$=$2
      ]]
    + _"{" "llist" "}" %[[
        -- $$=$0 append($2)
        $$=$2
      ]]
    + _"(" "lexps" ")" %[[
        $$=$2
      ]]
    + _"item" %[[
        $$=$1
      ]]
    ;

  rlist
    = _
    + _"exp"
    + _"exp" "," "rlist" %[[
        -- $$=$0 append($1, $3)
        -- create($rlist) append($1, (table.unpack or unpack)($3))
        -- $$=$0 append($3, $1)
      ]]
    ;

  llist
    = _
    + _"exp"
    + _"llist" "," "exp" %[[
        -- $$=$0 append($1, $3)
        $$=$1 append($3)
      ]]
    ;

  lexps
    = _
    + _"lexps" "exp" %[[
        $$=$1 append($2)
      ]]
    ;

  if_stmt
    = _"if exp then block" "{elseif exp then block}" "[else block]" "end"
    ;

  ["if exp then block"]
    = _"if" "exp" "then" "exp"
    ;

  ["{elseif exp then block}"]
    = _ %[[
        $$=create(${"elseif exp then block"})
      ]]
    + _"{elseif exp then block}" "elseif exp then block" %[[
        $$=$1 append($2)
      ]]
    ;

  ["elseif exp then block"]
    = _"elseif" "exp" "then" "exp" %[[
        $$=$1 append($2, $4)
      ]]
    ;

  ["[else block]"]
    = _ %[[
        $$=create($else)
      ]]
    + _"else" "exp" %[[
        $$=$1 append($2)
      ]]
    ;

}))
for _, message in c:ipairs() do
  if verbose or not message:find "^%[info%]" then
    print(message)
  end
end

local parser_filename = "test-gen-parser3-parser.lua"
local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(g, a))
out:close()

local R = assert(assert(loadfile(regexp_filename))())
local P = assert(assert(loadfile(parser_filename))())
local r = R([[
if () then {a} elseif () then {b} elseif () then {c} else {z} end
]], "@test", P.max_terminal_symbol, P())
local symbol_names = P.symbol_names

local function dump(u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  io.write(("  "):rep(n), ('<node name="%s"'):format(symbol_names[u[0]]))
  if u.v ~= nil then
    io.write((' v="%s"'):format(u.v))
  end
  if #u == 0 then
    io.write '/>\n'
  else
    io.write '>\n'
    for _, v in ipairs(u) do
      dump(v, n)
    end
    io.write(("  "):rep(n), '</node>\n')
  end

end
if verbose then
  dump(r)
end
