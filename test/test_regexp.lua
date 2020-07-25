-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local node = require "dromozoa.regexp.node"
local to_pattern = require "dromozoa.regexp.node.to_pattern"
local automaton = require "dromozoa.regexp.automaton"
local write_graphviz = require "dromozoa.regexp.automaton.write_graphviz"

local P = node.pattern
local R = node.range
local S = node.set

local p = (R"ac" * "abc") ^"*"
print(to_pattern(p))
write_graphviz(p:to_nfa(automaton.nfa(), 1), io.open("test.dot", "w")):close()
