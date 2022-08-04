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

local array = require "dromozoa.array"
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

local buffer = array()
for _, message in conflictions:ipairs() do
  buffer:append(message, "\n")
end

local set_of_items = data.lr0_set_of_items
local transitions = data.transitions

for i, items in set_of_items:ipairs() do
  buffer:append(("="):rep(75), "\nI_", i, "\n")
  for _, item in items:ipairs() do
    local production = g.productions[item.index]
    buffer:append("  ", g.symbol_names:get(production.head), " ->")
    for j, symbol in production.body:ipairs() do
      if j == item.dot then
        buffer:append " ."
      end
      buffer:append(" ", g.symbol_names:get(symbol))
    end
    if production.body:get(item.dot) == nil then
      buffer:append " ."
    end
    buffer:append "\n"
  end
  if not transitions[i]:empty() then
    buffer:append "\n"
  end
  for symbol, j in transitions[i]:pairs() do
    buffer:append("  I_", i, " -> I_", j, " ", g.symbol_names:get(symbol), "\n")
  end
end
buffer:append(("="):rep(75), "\n")

-- print(buffer:concat())
assert(buffer:concat() == [[
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

  I_5 -> I_9 E
  I_5 -> I_3 T
  I_5 -> I_4 F
  I_5 -> I_5 (
  I_5 -> I_6 id
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

  I_7 -> I_10 T
  I_7 -> I_4 F
  I_7 -> I_5 (
  I_7 -> I_6 id
===========================================================================
I_8
  T -> T * . F
  F -> . ( E )
  F -> . id

  I_8 -> I_11 F
  I_8 -> I_5 (
  I_8 -> I_6 id
===========================================================================
I_9
  F -> ( E . )
  E -> E . + T

  I_9 -> I_12 )
  I_9 -> I_7 +
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
