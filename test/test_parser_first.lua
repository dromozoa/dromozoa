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

-- P.222 Example 4.30
local g, actions, conflictions, data = lalr(grammar({ "+", "*", "(", ")", "id" }, {
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
}))

local buffer = list()
for _, message in conflictions:ipairs() do
  buffer:append(message, "\n")
end

local max_terminal_symbol = g.max_terminal_symbol
local symbol_names = g.symbol_names
local symbol_table = {}
for i = max_terminal_symbol + 2, symbol_names:size() do
  symbol_table[symbol_names:get(i)] = i
end
local first_table = g.first_table

for _, name in ipairs { "F", "T", "E", "E'", "T'" } do
  buffer:append("FIRST(", name, ") = { ")
  local first = first_table[symbol_table[name]]
  local i = 0
  for _, k in first:ipairs() do
    i = i + 1
    if i > 1 then
      buffer:append ", "
    end
    if k == 0 then
      buffer:append "e"
    else
      buffer:append(g.symbol_names:get(k))
    end
  end
  buffer:append " }\n"
end

-- print(buffer:concat())
assert(buffer:concat() == [[
FIRST(F) = { (, id }
FIRST(T) = { (, id }
FIRST(E) = { (, id }
FIRST(E') = { +, e }
FIRST(T') = { *, e }
]])
