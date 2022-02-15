-- Copyright (C) 2020-2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local graph = require "dromozoa.regexp.graph"

local function map_to_seq(map)
  local seq = {}
  for index, state in pairs(map) do
    seq[#seq + 1] = { index = index, state = state }
  end
  table.sort(seq, function (a, b) return a.index < b.index end)
  local key = {}
  for i = 1, #seq do
    key[i] = seq[i].index
  end
  seq.key = table.concat(key, ",")
  return seq
end

local function visit(u, map, state_indices)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if not transition.set then
      local v = transition.v
      local vid = state_indices[v]
      map[vid] = v
      visit(v, map, state_indices)
    end
  end
end

local function epsilon_closure(u, epsilon_closures, state_indices)
  local seq = epsilon_closures[u]
  if not seq then
    local map = { [state_indices[u]] = u }
    visit(u, map, state_indices)
    seq = map_to_seq(map)
    epsilon_closures[u] = seq
  end
  return seq
end

local function visit(useq, new_states, epsilon_closures, state_indices, color)
  local new_transitions = {}
  local new_transition_map = {}

  color[useq] = 1

  for byte = 0x00, 0xFF do
    local vmap = {}
    for i = 1, #useq do
      local transitions = useq[i].state.transitions
      for j = 1, #transitions do
        local transition = transitions[j]
        local set = transition.set
        if set and set[byte] then
          local seq = epsilon_closure(transition.v, epsilon_closures, state_indices)
          for k = 1, #seq do
            local vobj = seq[k]
            vmap[vobj.index] = vobj.state
          end
        end
      end
    end
    if next(vmap) then
      local vseq = map_to_seq(vmap)
      local vkey = vseq.key
      local vnew = new_states[vkey]
      if not vnew then
        vnew = { state = graph.new_state(), seq = vseq }
        new_states[vkey] = vnew
      end
      local new_transition = new_transition_map[vnew]
      if not new_transition then
        new_transition = { v = vnew, set = { [byte] = true } }
        new_transitions[#new_transitions + 1] = new_transition
        new_transition_map[vnew] = new_transition
      else
        new_transition.set[byte] = true
      end
    end
  end

  local ukey = useq.key
  local uobj = new_states[ukey]
  local unew = uobj.state
  for i = 1, #new_transitions do
    local new_transition = new_transitions[i]
    local vobj = new_transition.v
    local vnew = vobj.state
    local vseq = vobj.seq
    graph.new_transition(unew, vnew, new_transition.set)

    local accept
    for i = 1, #vseq do
      local yid = vseq[i].index
      local y = vseq[i].state
      local a = y.accept
      if a and (not accept or accept > a) then
        accept = a
      end
    end
    vnew.accept = accept

    if not color[vseq] then
      visit(vseq, new_states, epsilon_closures, state_indices, color)
    end
  end

  color[useq] = 2
end

return function (u)
  local state_indices = graph.create_state_indices(u)
  local epsilon_closures = {}
  local useq = epsilon_closure(u, epsilon_closures, state_indices)
  local unew = graph.new_state()
  local new_states = { [useq.key] = { state = unew, seq = useq } }
  visit(useq, new_states, epsilon_closures, state_indices, {})
  return unew
end
