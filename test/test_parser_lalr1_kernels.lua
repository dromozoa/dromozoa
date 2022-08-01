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

-- P.273
local g = grammar({ "=", "*", "id" }, {
  S = _"L" "=" "R"
    + _"R";
  L = _"*" "R"
    + _"id";
  R = _"L";
})

g.first_table = generate.first_table(generate.eliminate_left_recursion(g))
local set_of_items, transitions = generate.lr0_items(g)
local set_of_items = generate.lalr1_kernels(g, set_of_items, transitions)

local buffer = list()
for i, items in ipairs(set_of_items) do
  buffer:append(("="):rep(75), "\n")
  buffer:append("I_", i - 1, "\n")
  buffer:append(("-"):rep(75), "\n")
  for _, item in items:ipairs() do
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
    buffer:append(", ", g.symbol_names[item.la], "\n")
  end
  buffer:append(("-"):rep(75), "\n")
  for _, symbol, j in transitions[i]:each() do
    buffer:append("I_", i - 1, " -> I_", j - 1, " ", g.symbol_names[symbol], "\n")
  end
end
buffer:append(("="):rep(75), "\n")

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
===========================================================================
I_0
---------------------------------------------------------------------------
S' -> . S, $
---------------------------------------------------------------------------
I_0 -> I_1 S
I_0 -> I_2 L
I_0 -> I_3 R
I_0 -> I_4 *
I_0 -> I_5 id
===========================================================================
I_1
---------------------------------------------------------------------------
S' -> S ., $
---------------------------------------------------------------------------
===========================================================================
I_2
---------------------------------------------------------------------------
S -> L . = R, $
R -> L ., $
---------------------------------------------------------------------------
I_2 -> I_6 =
===========================================================================
I_3
---------------------------------------------------------------------------
S -> R ., $
---------------------------------------------------------------------------
===========================================================================
I_4
---------------------------------------------------------------------------
L -> * . R, =
L -> * . R, $
---------------------------------------------------------------------------
I_4 -> I_7 R
I_4 -> I_8 L
I_4 -> I_4 *
I_4 -> I_5 id
===========================================================================
I_5
---------------------------------------------------------------------------
L -> id ., =
L -> id ., $
---------------------------------------------------------------------------
===========================================================================
I_6
---------------------------------------------------------------------------
S -> L = . R, $
---------------------------------------------------------------------------
I_6 -> I_9 R
I_6 -> I_8 L
I_6 -> I_4 *
I_6 -> I_5 id
===========================================================================
I_7
---------------------------------------------------------------------------
L -> * R ., =
L -> * R ., $
---------------------------------------------------------------------------
===========================================================================
I_8
---------------------------------------------------------------------------
R -> L ., =
R -> L ., $
---------------------------------------------------------------------------
===========================================================================
I_9
---------------------------------------------------------------------------
S -> L = R ., $
---------------------------------------------------------------------------
===========================================================================
]])
