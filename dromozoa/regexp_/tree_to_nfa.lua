-- Copyright (C) 2021 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local function tree_to_nfa(node, transitions, action_states)
  local code = node[0]
  if code == "[" then
    local u = transitions:new_state()
    local v = transitions:new_state()
    transitions:set_transitions(u, v, node[1])
    return u, v
  elseif code == "*" then
    local au, av = tree_to_nfa(node[1], transitions, action_states)
    local u = transitions:new_state()
    local v = transitions:new_state()
    transitions:set_epsilon_transition(u, au)
    transitions:set_epsilon_transition(u, v)
    transitions:set_epsilon_transition(av, v)
    transitions:set_epsilon_transition(av, au)
    return u, v
  elseif code == "?" then
    local au, av = tree_to_nfa(node[1], transitions, action_states)
    local u = transitions:new_state()
    local v = transitions:new_state()
    transitions:set_epsilon_transition(u, au)
    transitions:set_epsilon_transition(u, v)
    transitions:set_epsilon_transition(av, v)
    return u, v
  elseif code == "." then
    local au, av = tree_to_nfa(node[1], transitions, action_states)
    local bu, bv = tree_to_nfa(node[2], transitions, action_states)
    transitions:set_epsilon_transition(av, bu)
    return au, bv
  elseif code == "|" then
    local au, av = tree_to_nfa(node[1], transitions, action_states)
    local bu, bv = tree_to_nfa(node[2], transitions, action_states)
    local u = transitions:new_state()
    local v = transitions:new_state()
    transitions:set_epsilon_transition(u, au)
    transitions:set_epsilon_transition(u, bu)
    transitions:set_epsilon_transition(av, v)
    transitions:set_epsilon_transition(bv, v)
    return u, v
  elseif code == "-" then
    error "not impl"
  elseif code == "@" then
    local u, v = tree_to_nfa(node[1], transitions, action_states)
    action_states[v] = node[2]
    return u, v
  end
end

return function (tree, transitions, accept)
  local action_states = {}
  local u, v = tree_to_nfa(tree, transitions, action_states)
  return {
    transitions = transitions;
    start_state = u;
    action_states = action_states;
    accept_states = { [v] = accept };
  }
end
