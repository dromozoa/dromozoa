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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local grammar = require "dromozoa.parser.grammar"

local _ = grammar.body

-- P.269
local g = grammar({ "c", "d" }, {
  S = _"C" "C";
  C = _"c" "C"
    | _"d";
})

g.first_table = grammar.first_table(grammar.eliminate_left_recursion(g))
local set_of_items, transitions = grammar.lalr1_items(g)
local t = grammar.lr1_construct_table(g, set_of_items, transitions)

local buffer = grammar.list()

buffer:append "|    |"
for i = 1, t.max_nonterminal_symbol do
  buffer:append((" %2s |"):format(t.symbol_names[i]))
end
buffer:append "\n"

for i, data in ipairs(t.actions) do
  buffer:append(("| %2d |"):format(i))
  for j = 1, t.max_nonterminal_symbol do
    local v = data[j]
    if not v then
      buffer:append "    |"
    else
      buffer:append " "
      if v <= t.max_state then
        buffer:append("s", v)
      else
        local v = v - t.max_state
        if v == 1 then
          buffer:append "ac"
        else
          buffer:append("r", v)
        end
      end
      buffer:append " |"
    end
  end
  buffer:append "\n"
end

io.write(table.concat(buffer))
