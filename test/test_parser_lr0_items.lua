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

-- P.246
local g = grammar({ "+", "*", "(", ")", "id" }, {
  E = _"E" "+" "T"
    + _"T";
  T = _"T" "*" "F"
    + _"F";
  F = _"(" "E" ")"
    + _"id";
})

local set_of_items, transitions = generate.lr0_items(g)

local buffer = list()
for i, items in ipairs(set_of_items) do
  buffer:append(("="):rep(75), "\n")
  buffer:append("I_", i - 1, "\n")
  buffer:append(("-"):rep(75), "\n")
  for _, item in ipairs(items) do
    local production = g.productions[item.index]
    buffer:append(g.symbol_names[production.head], " ->")
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
  buffer:append(("-"):rep(75), "\n")
  for symbol, j in pairs(transitions[i]) do
    buffer:append("I_", i - 1, " -> I_", j - 1, " ", g.symbol_names[symbol], "\n")
  end
end
buffer:append(("="):rep(75), "\n")

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
===========================================================================
I_0
---------------------------------------------------------------------------
E' -> . E
E -> . E + T
E -> . T
T -> . T * F
T -> . F
F -> . ( E )
F -> . id
---------------------------------------------------------------------------
I_0 -> I_1 (
I_0 -> I_2 id
I_0 -> I_3 E
I_0 -> I_4 T
I_0 -> I_5 F
===========================================================================
I_1
---------------------------------------------------------------------------
F -> ( . E )
E -> . E + T
E -> . T
T -> . T * F
T -> . F
F -> . ( E )
F -> . id
---------------------------------------------------------------------------
I_1 -> I_1 (
I_1 -> I_2 id
I_1 -> I_6 E
I_1 -> I_4 T
I_1 -> I_5 F
===========================================================================
I_2
---------------------------------------------------------------------------
F -> id .
---------------------------------------------------------------------------
===========================================================================
I_3
---------------------------------------------------------------------------
E' -> E .
E -> E . + T
---------------------------------------------------------------------------
I_3 -> I_7 +
===========================================================================
I_4
---------------------------------------------------------------------------
E -> T .
T -> T . * F
---------------------------------------------------------------------------
I_4 -> I_8 *
===========================================================================
I_5
---------------------------------------------------------------------------
T -> F .
---------------------------------------------------------------------------
===========================================================================
I_6
---------------------------------------------------------------------------
F -> ( E . )
E -> E . + T
---------------------------------------------------------------------------
I_6 -> I_7 +
I_6 -> I_9 )
===========================================================================
I_7
---------------------------------------------------------------------------
E -> E + . T
T -> . T * F
T -> . F
F -> . ( E )
F -> . id
---------------------------------------------------------------------------
I_7 -> I_1 (
I_7 -> I_2 id
I_7 -> I_10 T
I_7 -> I_5 F
===========================================================================
I_8
---------------------------------------------------------------------------
T -> T * . F
F -> . ( E )
F -> . id
---------------------------------------------------------------------------
I_8 -> I_1 (
I_8 -> I_2 id
I_8 -> I_11 F
===========================================================================
I_9
---------------------------------------------------------------------------
F -> ( E ) .
---------------------------------------------------------------------------
===========================================================================
I_10
---------------------------------------------------------------------------
E -> E + T .
T -> T . * F
---------------------------------------------------------------------------
I_10 -> I_8 *
===========================================================================
I_11
---------------------------------------------------------------------------
T -> T * F .
---------------------------------------------------------------------------
===========================================================================
]])
