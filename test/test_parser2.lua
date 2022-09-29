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

local token_names = {}

local regexp_filename = "out/test_parser2_regexp.lua"
local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
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
})
out:close()

local _ = parser.grammar.body
local left = parser.grammar.left
local right = parser.grammar.right

local g, a, c = parser.lalr(parser.grammar(token_names, {
  left "+" "-";
  left "*" "/";
  right "UNM";

  E = _"E" "+" "E" %[[$$.v=$1.v+$3.v $$.code=$1.code..$3.code.."ADD\n"]]
    + _"E" "-" "E" %[[$$.v=$1.v-$3.v $$.code=$1.code..$3.code.."SUB\n"]]
    + _"E" "*" "E" %[[$$.v=$1.v*$3.v $$.code=$1.code..$3.code.."MUL\n"]]
    + _"E" "/" "E" %[[$$.v=$1.v/$3.v $$.code=$1.code..$3.code.."DIV\n"]]
    + _"(" "E" ")" %[[$$.v=$2.v $$.code=$2.code]]
    + _"-" "E" :prec "UNM" %[[$$.v=-$2.v $$.code=$2.code.."NEG\n"]]
    + _"number" %[[$$.v=tonumber($1.v) $$.code="PUSH "..$1.v.."\n"]]
    ;
}))
for _, message in ipairs(c) do
  if verbose or not message:find "^%[info%]" then
    print(message)
  end
end

local parser_filename = "out/test_parser2_parser.lua"
local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(g, a))
out:close()

local R = assert(assert(loadfile(regexp_filename))())
local P = assert(assert(loadfile(parser_filename))())

local r = R([[
2 + 3 * 4 - 6 / -3 + (1 + 2) * 3
]], "@test", P.max_terminal_symbol, P())
if verbose then
  print(r.v)
  print(r.code)
end
assert(r.v == 25)
assert(r.code == [[
PUSH 2
PUSH 3
PUSH 4
MUL
ADD
PUSH 6
PUSH 3
NEG
DIV
SUB
PUSH 1
PUSH 2
ADD
PUSH 3
MUL
ADD
]])
