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
local lalr = require "dromozoa.parser.lalr"

local _ = grammar.body

-- P.246 Example 4.41
local g = grammar({ "+", "*", "(", ")", "id" }, {
  E = _"E" "+" "T"
    + _"T";
  T = _"T" "*" "F"
    + _"F";
  F = _"(" "E" ")"
    + _"id";
})
local actions, conflictions, data = lalr(g)
local buffer = list()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end
local set_of_items = data.lr0_set_of_items
local transitions = data.transitions

local buffer = list()
for i, items in set_of_items:ipairs() do
  buffer:append(("="):rep(75), "\nI_", i - 1, "\n")
  for _, item in items:ipairs() do
    local production = g.productions[item.index]
    buffer:append("  ", g.symbol_names[production.head], " ->")
    for j, symbol in ipairs(production.body) do
      if j == item.dot then
        buffer:append " ."
      end
      buffer:append(" ", g.symbol_names[symbol])
    end
    if not production.body[item.dot] then
      buffer:append " ."
    end
    buffer:append "\n"
  end
  buffer:append "\n"
  for symbol, j in transitions[i]:pairs() do
    buffer:append("  I_", i - 1, " -> I_", j - 1, " ", g.symbol_names[symbol], "\n")
  end
end
buffer:append(("="):rep(75), "\n")

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
===========================================================================
I_0
  E' -> . E
  E -> . E + T
  E -> . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_0 -> I_1 E
  I_0 -> I_2 T
  I_0 -> I_3 F
  I_0 -> I_4 (
  I_0 -> I_5 id
===========================================================================
I_1
  E' -> E .
  E -> E . + T

  I_1 -> I_6 +
===========================================================================
I_2
  E -> T .
  T -> T . * F

  I_2 -> I_7 *
===========================================================================
I_3
  T -> F .

===========================================================================
I_4
  F -> ( . E )
  E -> . E + T
  E -> . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_4 -> I_8 E
  I_4 -> I_2 T
  I_4 -> I_3 F
  I_4 -> I_4 (
  I_4 -> I_5 id
===========================================================================
I_5
  F -> id .

===========================================================================
I_6
  E -> E + . T
  T -> . T * F
  T -> . F
  F -> . ( E )
  F -> . id

  I_6 -> I_9 T
  I_6 -> I_3 F
  I_6 -> I_4 (
  I_6 -> I_5 id
===========================================================================
I_7
  T -> T * . F
  F -> . ( E )
  F -> . id

  I_7 -> I_10 F
  I_7 -> I_4 (
  I_7 -> I_5 id
===========================================================================
I_8
  F -> ( E . )
  E -> E . + T

  I_8 -> I_11 )
  I_8 -> I_6 +
===========================================================================
I_9
  E -> E + T .
  T -> T . * F

  I_9 -> I_7 *
===========================================================================
I_10
  T -> T * F .

===========================================================================
I_11
  F -> ( E ) .

===========================================================================
]])
