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
local dumper = require "dromozoa.commons.dumper"

local transition_table = require "dromozoa.regexp.transition_table"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"

-- ext
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = node.pattern
local R = node.range
local S = node.set

local p = P"ab" / 1 * "cd"
local p = P"abc" / 1
local p = P"abc" * "def" / 1
local p = (P"abc" / 1 * ("abc" * S"def") / 2)^"?"
-- print(dumper.encode(p, { pretty = true, stable = true }))

local transitions = transition_table.new(2)
local start_state, action_states, accept_states = tree_to_nfa(p, transitions, 1)

local nfa = {
  transitions = transitions;
  start_state = start_state;
  action_states = action_states;
  accept_states = accept_states;
}

local out = assert(io.open("test.dot", "w"))
write_graphviz(nfa, out)
out:close()

local start_state, action_states, accept_states = nfa_to_dfa(nfa, transition_table.new(0))

os.exit()

assert(to_pattern(P"abc"^"*") == "(abc)*")
assert(to_pattern(S"abc"^"*") == "[a-c]*")

local p = (R"ac" * "abc") ^"*"
print(to_pattern(p))

local transitions = matrix.new(2)
local start_state, accept_states = tree_to_nfa(p, transitions, 1)
local nfa = automaton.new()
nfa.max_state = transitions.max_state
nfa.transitions = transitions
nfa.start_state = start_state
nfa.accept_states = accept_states

-- local nfa = p:to_nfa(automaton.new(), 1)
write_graphviz(nfa, io.open("test.dot", "w")):close()

local dfa = nfa:to_dfa()
write_graphviz(dfa, io.open("test.dot", "w")):close()

local p = P"if" + "else" + "elseif"
local nfa = p:to_nfa(automaton.new(), 1)
print(to_pattern(p))
local dfa = nfa:to_dfa()
write_graphviz(dfa, io.open("test.dot", "w")):close()
local dfa = dfa:minimize()
write_graphviz(dfa, io.open("test.dot", "w")):close()

local p1 = R"ac"^"+"
local p2 = P"abc"
local dfa1 = p1:to_nfa(automaton.new(), 1):to_dfa():minimize()
local dfa2 = p2:to_nfa(automaton.new(), 1):to_dfa():minimize()
write_graphviz(dfa1, io.open("test1.dot", "w")):close()
write_graphviz(dfa2, io.open("test2.dot", "w")):close()
local dfa3 = dfa1:difference(dfa2)
write_graphviz(dfa3, io.open("test3.dot", "w")):close()

local dfa4 = automaton.new():remove_unreachable_states(dfa3):minimize()
-- local dfa4 = dfa3:remove_unreachable_states():minimize()
write_graphviz(dfa4, io.open("test4.dot", "w")):close()
