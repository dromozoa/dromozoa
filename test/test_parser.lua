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

local actions, conflictions = lalr(g)
local code = compile(g, actions)
local filename = "test-gen-parser.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local buffer = list()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end

local parser = assert(assert(loadfile(filename))())
local p = parser()
p { [0] = token_table["id"] }
p { [0] = token_table["+"] }
p { [0] = token_table["("] }
p { [0] = token_table["id"] }
p { [0] = token_table["+"] }
p { [0] = token_table["id"] }
p { [0] = token_table[")"] }
p { [0] = token_table["*"] }
p { [0] = token_table["id"] }
local tree = p { [0] = #token_names + 1 }

local function dump(u, depth)
  depth = depth == nil and 0 or depth + 1
  buffer:append(("  "):rep(depth), p.symbol_names[u[0]], "\n")
  for _, v in ipairs(u) do
    dump(v, depth)
  end
end
dump(tree)

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
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
