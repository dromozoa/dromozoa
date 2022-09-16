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

-- P.214 Example 4.20
local g, actions, conflictions, data = lalr(grammar({ "a", "b", "c", "d" }, {
  S = _"A" "a"
    + _"b";
  A = _"A" "c"
    + _"S" "d"
    + _();
}))

local buffer = array()
for _, message in ipairs(conflictions) do
  buffer:append(message, "\n")
end

local g = data.grammar_without_left_recursion

for _, production in ipairs(g.productions) do
  buffer:append(g.symbol_names[production.head], " ->")
  for _, symbol in ipairs(production.body) do
    buffer:append(" ", g.symbol_names[symbol])
  end
  buffer:append "\n"
end

-- print(buffer:concat())
assert(buffer:concat() == [[
S' -> S
S -> A a
S -> b
A -> b d A'
A -> A'
A' -> c A'
A' -> a d A'
A' ->
]])
