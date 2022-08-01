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
local grammar = require "dromozoa.parser.grammar"
local generate = require "dromozoa.parser.generate"

local _ = grammar.body

-- P.222
local g = grammar({ "+", "*", "(", ")", "id" }, {
  E = _"T" "E'";
  ["E'"]
    = _"+" "T" "E'"
    + _;
  T = _"F" "T'";
  ["T'"]
    = _"*" "F" "T'"
    + _;
  F = _"(" "E" ")"
    + _"id";
})
local first_table = generate.first_table(g)
g.first_table = first_table

local buffer = list()
for _, name in ipairs { "F", "T", "E", "E'", "T'" } do
  buffer:append("FIRST(", name, ") = { ")
  local first = generate.first_symbol(g, g.symbol_table[name])
  local i = 0
  for _, k in first:each() do
    i = i + 1
    if i > 1 then
      buffer:append ", "
    end
    if k == 0 then
      buffer:append "e"
    else
      buffer:append(g.symbol_names[k])
    end
  end
  buffer:append " }\n"
end

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
FIRST(F) = { (, id }
FIRST(T) = { (, id }
FIRST(E) = { (, id }
FIRST(E') = { +, e }
FIRST(T') = { *, e }
]])
