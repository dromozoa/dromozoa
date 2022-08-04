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
local g, actions, conflictions, data = lalr(grammar({ "=", "*", "id" }, {
  S = _"L" "=" "R"
    + _"R";
  L = _"*" "R"
    + _"id";
  R = _"L";
}))

local buffer = list()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end

local set_of_items = data.lalr1_set_of_items

for i, items in set_of_items:ipairs() do
  buffer:append(("="):rep(75), "\nI_", i, "\n")
  for _, item in items:ipairs() do
    -- カーネル項だけを出力する
    if item.index == 1 or item.dot > 1 then
      local production = g.productions[item.index]
      buffer:append("  ", g.symbol_names[production.head], " ->")
      for j, symbol in production.body:ipairs() do
        if j == item.dot then
          buffer:append " ."
        end
        buffer:append(" ", g.symbol_names[symbol])
      end
      if production.body:get(item.dot) == nil then
        buffer:append " ."
      end
      buffer:append(", ", g.symbol_names[item.la], "\n")
    end
  end
end
buffer:append(("="):rep(75), "\n")

-- print(buffer:concat())
assert(buffer:concat() == [[
===========================================================================
I_1
  S' -> . S, $
===========================================================================
I_2
  S' -> S ., $
===========================================================================
I_3
  S -> L . = R, $
  R -> L ., $
===========================================================================
I_4
  S -> R ., $
===========================================================================
I_5
  L -> * . R, =
  L -> * . R, $
===========================================================================
I_6
  L -> id ., =
  L -> id ., $
===========================================================================
I_7
  S -> L = . R, $
===========================================================================
I_8
  L -> * R ., =
  L -> * R ., $
===========================================================================
I_9
  R -> L ., =
  R -> L ., $
===========================================================================
I_10
  S -> L = R ., $
===========================================================================
]])
