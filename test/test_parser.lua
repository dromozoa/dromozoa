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

local list = require "dromozoa.list"
local compile = require "dromozoa.parser.compile"
local grammar = require "dromozoa.parser.grammar"
local parser = require "dromozoa.parser.parser"

local _ = grammar.body
local left = grammar.left

-- local g = grammar({}):???():compile()
-- compile(g)

local token_names = list("id", "+", "*", "(", ")")
local token_table = {}
for i, v in token_names:ipairs() do
  token_table[v] = i
end

local g = grammar(token_names, {
  left "+";
  left "*";

  E = _"E" "+" "E"
    + _"E" "*" "E"
    + _"(" "E" ")"
    + _"id";
})

local buffer = list()
local p = parser(g)
for _, message in ipairs(p.conflictions) do
  buffer:append(message, "\n")
end

local code = compile(p)

local filename = "test-gen-parser.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local P = assert(assert(loadfile(filename))())
local p = P()
p { [0] = token_table["id"] }
p { [0] = token_table["+"] }
p { [0] = token_table["("] }
p { [0] = token_table["id"] }
p { [0] = token_table["+"] }
p { [0] = token_table["id"] }
p { [0] = token_table[")"] }
p { [0] = token_table["*"] }
p { [0] = token_table["id"] }
local x = p { [0] = #token_names + 1 }

local function dump(x, depth)
  if depth == nil then
    depth = 0
  else
    depth = depth + 1
  end
  buffer:append(("  "):rep(depth))
  buffer:append(p.symbol_names[x[0]])
  buffer:append("\n")
  for _, y in ipairs(x) do
    dump(y, depth)
  end
end
dump(x)

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
shift(5) / reduce(2) conflict resolved as reduce(2): precedence 1 == 1 associativity left at state(8) production(2) symbol(+)
shift(6) / reduce(2) conflict resolved as shift(6): precedence 2 > 1 at state(8) production(2) symbol(*)
shift(5) / reduce(3) conflict resolved as reduce(3): precedence 1 < 2 at state(9) production(3) symbol(+)
shift(6) / reduce(3) conflict resolved as reduce(3): precedence 2 == 2 associativity left at state(9) production(3) symbol(*)
E
  E
    id
  +
  E
    E
      (
      E
        E
          id
        +
        E
          id
      )
    *
    E
      id
]])
