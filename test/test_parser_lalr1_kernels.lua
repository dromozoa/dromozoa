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

-- P.273 Example 4.64
local g = grammar({ "=", "*", "id" }, {
  S = _"L" "=" "R"
    + _"R";
  L = _"*" "R"
    + _"id";
  R = _"L";
})
local actions, conflictions, data = lalr(g)
local buffer = list()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end
local set_of_items = data.lalr1_set_of_items

for i, items in ipairs(set_of_items) do
  buffer:append(("="):rep(75), "\nI_", i, "\n", ("-"):rep(75), "\n")
  for _, item in items:ipairs() do
    -- カーネル項だけを出力する
    if item.index == 1 or item.dot > 1 then
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
  end
  buffer:append(("-"):rep(75), "\n")
  for symbol, j in data.transitions[i]:pairs() do
    buffer:append("I_", i, " -> I_", j, " ", g.symbol_names[symbol], "\n")
  end
end
buffer:append(("="):rep(75), "\n")

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
===========================================================================
I_1
---------------------------------------------------------------------------
S' -> . S, $
---------------------------------------------------------------------------
I_1 -> I_2 S
I_1 -> I_3 L
I_1 -> I_4 R
I_1 -> I_5 *
I_1 -> I_6 id
===========================================================================
I_2
---------------------------------------------------------------------------
S' -> S ., $
---------------------------------------------------------------------------
===========================================================================
I_3
---------------------------------------------------------------------------
S -> L . = R, $
R -> L ., $
---------------------------------------------------------------------------
I_3 -> I_7 =
===========================================================================
I_4
---------------------------------------------------------------------------
S -> R ., $
---------------------------------------------------------------------------
===========================================================================
I_5
---------------------------------------------------------------------------
L -> * . R, =
L -> * . R, $
---------------------------------------------------------------------------
I_5 -> I_8 R
I_5 -> I_9 L
I_5 -> I_5 *
I_5 -> I_6 id
===========================================================================
I_6
---------------------------------------------------------------------------
L -> id ., =
L -> id ., $
---------------------------------------------------------------------------
===========================================================================
I_7
---------------------------------------------------------------------------
S -> L = . R, $
---------------------------------------------------------------------------
I_7 -> I_10 R
I_7 -> I_9 L
I_7 -> I_5 *
I_7 -> I_6 id
===========================================================================
I_8
---------------------------------------------------------------------------
L -> * R ., =
L -> * R ., $
---------------------------------------------------------------------------
===========================================================================
I_9
---------------------------------------------------------------------------
R -> L ., =
R -> L ., $
---------------------------------------------------------------------------
===========================================================================
I_10
---------------------------------------------------------------------------
S -> L = R ., $
---------------------------------------------------------------------------
===========================================================================
]])
