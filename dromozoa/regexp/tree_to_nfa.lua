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

local function tree_to_nfa(node, transitions)
  local code = node[0]
  if code == "[" then
    local u = transitions:add_state()
    local v = transitions:add_state()
    transitions:add_transition(u, v, node[1])
    return u, v
  elseif code == "*" then
    local au, av = tree_to_nfa(node[1], transitions)
    local u = transitions:add_state()
    local v = transitions:add_state()
    transitions:add_epsilon_transition(u, au)
    transitions:add_epsilon_transition(u, v)
    transitions:add_epsilon_transition(av, v)
    transitions:add_epsilon_transition(av, au)
    return u, v
  elseif code == "?" then
    local au, av = tree_to_nfa(node[1], transitions)
    local u = transitions:add_state()
    local v = transitions:add_state()
    transitions:add_epsilon_transition(u, au)
    transitions:add_epsilon_transition(u, v)
    transitions:add_epsilon_transition(av, v)
    return u, v
  elseif code == "." then
    local au, av = tree_to_nfa(node[1], transitions)
    local bu, bv = tree_to_nfa(node[2], transitions)
    transitions:add_epsilon_transition(av, bu)
    return au, bv
  elseif code == "|" then
    local au, av = tree_to_nfa(node[1], transitions)
    local bu, bv = tree_to_nfa(node[2], transitions)
    local u = transitions:add_state()
    local v = transitions:add_state()
    transitions:add_epsilon_transition(u, au)
    transitions:add_epsilon_transition(u, bu)
    transitions:add_epsilon_transition(av, v)
    transitions:add_epsilon_transition(bv, v)
    return u, v
  elseif code == "-" then
    error "not impl"
  elseif code == "/" then
    error "not impl"
  end
end

return function (tree, transitions, accept)
  local u, v = tree_to_nfa(tree, transitions)
  return u, { [v] = accept }
end
