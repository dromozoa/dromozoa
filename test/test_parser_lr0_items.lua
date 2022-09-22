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

local append = require "dromozoa.append"
local grammar = require "dromozoa.parser.grammar"
local lalr = require "dromozoa.parser.lalr"

local _ = grammar.body

-- P.244 Example 4.40
local g, actions, conflictions, data = lalr(grammar({ "+", "*", "(", ")", "id" }, {
  E = _"E" "+" "T"
    + _"T";
  T = _"T" "*" "F"
    + _"F";
  F = _"(" "E" ")"
    + _"id";
}))

local buffer = {}
for _, message in ipairs(conflictions) do
  append(buffer, message, "\n")
end

local set_of_items = data.lr0_set_of_items
local transitions = data.transitions

for i, items in ipairs(set_of_items) do
  append(buffer, ("="):rep(75), "\nI_", i, "\n")
  for _, item in ipairs(items) do
    local production = g.productions[item.index]
    append(buffer, "  ", g.symbol_names[production.head], " ->")
    for j, symbol in ipairs(production.body) do
      if j == item.dot then
        append(buffer, " .")
      end
      append(buffer, " ", g.symbol_names[symbol])
    end
    if production.body[item.dot] == nil then
      append(buffer, " .")
    end
    append(buffer, "\n")
  end
  if next(transitions[i]) then
    append(buffer, "\n")
  end

  local transition = {}
  for symbol, j in pairs(transitions[i]) do
    transition[#transition + 1] = { symbol = symbol, j = j }
  end
  table.sort(transition, function (a, b) return a.j < b.j end)
  for _, t in ipairs(transition) do
    append(buffer, "  I_", i, " -> I_", t.j, " ", g.symbol_names[t.symbol], "\n")
  end
end
append(buffer, ("="):rep(75), "\n")

assert(table.concat(buffer) == [[
===========================================================================
I_1
  E' -> . E
  E -> . E + T
  E -> . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_1 -> I_2 E
  I_1 -> I_3 T
  I_1 -> I_4 F
  I_1 -> I_5 (
  I_1 -> I_6 id
===========================================================================
I_2
  E' -> E .
  E -> E . + T

  I_2 -> I_7 +
===========================================================================
I_3
  E -> T .
  T -> T . * F

  I_3 -> I_8 *
===========================================================================
I_4
  T -> F .
===========================================================================
I_5
  F -> ( . E )
  E -> . E + T
  E -> . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_5 -> I_3 T
  I_5 -> I_4 F
  I_5 -> I_5 (
  I_5 -> I_6 id
  I_5 -> I_9 E
===========================================================================
I_6
  F -> id .
===========================================================================
I_7
  E -> E + . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_7 -> I_4 F
  I_7 -> I_5 (
  I_7 -> I_6 id
  I_7 -> I_10 T
===========================================================================
I_8
  T -> T * . F
  F -> . ( E )
  F -> . id

  I_8 -> I_5 (
  I_8 -> I_6 id
  I_8 -> I_11 F
===========================================================================
I_9
  F -> ( E . )
  E -> E . + T

  I_9 -> I_7 +
  I_9 -> I_12 )
===========================================================================
I_10
  E -> E + T .
  T -> T . * F

  I_10 -> I_8 *
===========================================================================
I_11
  T -> T * F .
===========================================================================
I_12
  F -> ( E ) .
===========================================================================
]])
