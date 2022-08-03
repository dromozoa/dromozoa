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
local tree_set = require "dromozoa.tree_set"
local grammar = require "dromozoa.parser.grammar"
local lalr = require "dromozoa.parser.lalr"

local _ = grammar.body

-- P.263 Example 4.54
local g = grammar({ "c", "d" }, {
  S = _"C" "C";
  C = _"c" "C"
    + _"d";
})

local actions, conflictions, data = lalr(g)
local buffer = list()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end

local set_of_items = data.lalr1_set_of_items

local items = lalr.lr1_closure(g, tree_set():insert { index = 1, dot = 1, la = g.max_terminal_symbol })

for i, items in ipairs(set_of_items) do
  buffer:append(("="):rep(75), "\nI_", i, "\n")
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
    buffer:append(", ", g.symbol_names[item.la], "\n")
  end
end
buffer:append(("="):rep(75), "\n")

--[[
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
]]

print(table.concat(buffer))
assert(table.concat(buffer) == [[
===========================================================================
I_1
  S' -> . S, $
  S -> . C C, $
  C -> . c C, c
  C -> . c C, d
  C -> . d, c
  C -> . d, d
===========================================================================
I_2
  S' -> S ., $
===========================================================================
I_3
  S -> C . C, $
  C -> . c C, $
  C -> . d, $
===========================================================================
I_4
  C -> c . C, c
  C -> c . C, d
  C -> c . C, $
  C -> . c C, c
  C -> . d, c
  C -> . c C, d
  C -> . d, d
  C -> . c C, $
  C -> . d, $
===========================================================================
I_5
  C -> d ., c
  C -> d ., d
  C -> d ., $
===========================================================================
I_6
  S -> C C ., $
===========================================================================
I_7
  C -> c C ., c
  C -> c C ., d
  C -> c C ., $
===========================================================================
]])
