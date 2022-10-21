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

local regexp_filename, parser_filename = ...
local token_names = {}

local _ = regexp.pattern

local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
  regexp.machine.lexer(token_names, {
    _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
      _{" \f\t\v"}*"+";
    }*"*";

    _"i32";
    _"i64";
    _"f32";
    _"f64";

    _"(";
    _")";
    _",";
    _"->";
  });
})
out:close()

local _ = parser.grammar.body

local grammar, actions, conflictions, data = parser.lalr(parser.grammar(token_names, {
  valtype
    = _"numtype"
    + _"functype";

  numtype
    = _"i32"
    + _"i64"
    + _"f32"
    + _"f64";

  functype
    = _"tuple" "->" "tuple";

  tuple
    = _"(" ")"
    + _"(" "tuplelist" ")";

  tuplelist
    = _"valtype"
    + _"tuplelist" "," "valtype";
}))

for _, message in ipairs(conflictions) do
  if message:sub(1, 6) == "[warn]" then
    print(message)
  end
end

local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(grammar, actions))
out:close()
