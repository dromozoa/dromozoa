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
local lalr = require "dromozoa.parser.lalr"

local _ = grammar.body
local left = grammar.left

local g, actions, conflictions = lalr(grammar({ "id", "+", "*", "(", ")" }, {
  left "+";
  left "*";

  E = _"E" "+" "E"
    + _"E" "*" "E"
    + _"(" "E" ")"
    + _"id";
}))

local buffer = list()
for _, message in conflictions:ipairs() do
  buffer:append(message, "\n")
end

local code = compile(g, actions)
local filename = "test-gen-parser.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local parser = assert(assert(loadfile(filename))())
local max_terminal_symbol = parser.max_terminal_symbol
local symbol_names = parser.symbol_names
local symbol_table = {}
for i = 1, max_terminal_symbol - 1 do
  symbol_table[symbol_names[i]] = i
end

local p = parser()
p { [0] = symbol_table["id"] }
p { [0] = symbol_table["+"] }
p { [0] = symbol_table["("] }
p { [0] = symbol_table["id"] }
p { [0] = symbol_table["+"] }
p { [0] = symbol_table["id"] }
p { [0] = symbol_table[")"] }
p { [0] = symbol_table["*"] }
p { [0] = symbol_table["id"] }
local tree = p { [0] = max_terminal_symbol }

local function dump(u, depth)
  depth = depth == nil and 0 or depth + 1
  buffer:append(("  "):rep(depth), symbol_names[u[0]], "\n")
  for _, v in ipairs(u) do
    dump(v, depth)
  end
end
dump(tree)

-- print(buffer:concat())
assert(buffer:concat() == [[
[info] conflict between production 2 and symbol + resolved as reduce (left +)
[info] conflict between production 2 and symbol * resolved as shift (+ < *)
[info] conflict between production 3 and symbol + resolved as reduce (+ < *)
[info] conflict between production 3 and symbol * resolved as reduce (left *)
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
