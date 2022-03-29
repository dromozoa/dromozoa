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

local difference = require "dromozoa.regexp.difference"
local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local pattern = require "dromozoa.regexp.pattern"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local data = {
  dfa1 = minimize(nfa_to_dfa(tree_to_nfa(P(1)^0)));
  dfa2 = minimize(nfa_to_dfa(tree_to_nfa(P(1)^0 * P"a" * P"b"^0 * P"c" * P(1)^0)));
}

for name, dfa in pairs(data) do
  local out = assert(io.open(("test-dfa-%s.dot"):format(name), "w"))
  write_graphviz(out, dfa)
  out:close()
end

local dfa = difference(data.dfa1, data.dfa2)

local out = assert(io.open("test-dfa.dot", "w"))
write_graphviz(out, dfa)
out:close()
