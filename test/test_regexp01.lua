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

local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local pattern = require "dromozoa.regexp.pattern"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"
local write_graphviz = require "dromozoa.regexp.write_graphviz"
local write_graphviz_tree = require "dromozoa.regexp.write_graphviz_tree"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local i = 0

local function test(root, check_start_accept)
  i = i + 1

  local out = assert(io.open(("test%04d-tree.dot"):format(i), "w"))
  write_graphviz_tree(out, root)
  out:close()

  local nfa = tree_to_nfa(root)

  local out = assert(io.open(("test%04d-nfa.dot"):format(i), "w"))
  write_graphviz(out, nfa)
  out:close()

  local dfa1 = nfa_to_dfa(nfa)

  local out = assert(io.open(("test%04d-dfa1.dot"):format(i), "w"))
  write_graphviz(out, dfa1)
  out:close()

  local dfa2 = minimize(dfa1)

  local out = assert(io.open(("test%04d-dfa2.dot"):format(i), "w"))
  write_graphviz(out, dfa2)
  out:close()

  if check_start_accept then
    assert(dfa1.accept)
    assert(dfa2.accept)
  end
end

test(P"abc"^0, true)
test(P"abc"^1)
test(P"abc"^2)
test(P"abc"^3)
test(P"x"^0, true)
test((R"ac" + R"af"/1 + R"az"/2)^1)
test(P"if" + P"else" + P"elseif" + P"end")
test(P"X" * P"a" * P"b" * P"c" + P"Y" * P"a" * P"b" * P"c")
test(P"X" * (P"a"/1) * P"b" * P"c" + P"Y" * (P"a"/2) * P"b" * P"c")
test(P"X" * P"a" * (P"b"/1) * P"c" + P"Y" * P"a" * (P"b"/2) * P"c")
test(P"X" * P"a" * P"b" * (P"c"/1) + P"Y" * P"a" * P"b" * (P"c"/2))
test(P(1) * P"abc" * (R"09" + S"abc"))
test(P(2) * P"abc"^0)
test((R"ac" * P"abc" + (P"d"/1 + R"df"/2) * P"def")^0, true)
test(R"07" * R"07"^-2 * P(1))
