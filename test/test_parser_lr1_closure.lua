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
local generate = require "dromozoa.parser.generate"

local _ = grammar.body

-- P.263
local g = grammar({ "c", "d" }, {
  S = _"C" "C";
  C = _"c" "C"
    + _"d";
})

local items = tree_set()
items:put { index = 1, dot = 1, la = g.max_terminal_symbol }
local items = generate.lr1_closure(g, items)

local buffer = list()
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

assert(table.concat(buffer) == [[
S' -> . S, $
S -> . C C, $
C -> . c C, c
C -> . c C, d
C -> . d, c
C -> . d, d
]])
