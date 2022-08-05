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
local regexp_code = regexp.compile {
  regexp.machine.lexer(token_names, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    _"(";
    _")";
    _"+";
    _"-";
    _"*";
    _"/";
    number = _["09"]*"+";
  });
}

local regexp_filename = "test-gen-parser2-regexp.lua"
local out = assert(io.open(regexp_filename, "w"))
out:write(regexp_code)
out:close()

local _ = parser.grammar.body
local left = parser.grammar.left
local right = parser.grammar.right

local g, a, c = parser.lalr(parser.grammar(token_names, {
  left "+" "-";
  left "*" "/";
  right "UNM";

  E = _"E" "+" "E" %[[$$.v = $1.v + $3.v]]
    + _"E" "-" "E" %[[$$.v = $1.v - $3.v]]
    + _"E" "*" "E" %[[$$.v = $1.v * $3.v]]
    + _"E" "/" "E" %[[$$.v = $1.v / $3.v]]
    + _"(" "E" ")" %[[$$.v = $2.v]]
    + _"-" "E" :prec "UNM" %[[$$.v = -$2.v]]
    + _"number" %[[$$.v = tonumber($1.v)]]
    ;
}))
for _, message in c:ipairs() do
  if verbose or not message:find "^%[info%]" then
    print(message)
  end
end

local parser_code = parser.compile(g, a)
local parser_filename = "test-gen-parser2-parser.lua"
local out = assert(io.open(parser_filename, "w"))
out:write(parser_code)
out:close()

local R = assert(assert(loadfile(regexp_filename))())
local P = assert(assert(loadfile(parser_filename))())

local p = P "@test"
local r = R([[
2 + 3 * 4 - 6 / -3
]], "@test", P.max_terminal_symbol, p)
if verbose then
  print(r.v)
end
assert(r.v == 16)
