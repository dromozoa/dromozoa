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

local fsm = require "dromozoa.regexp.fsm"

local function visit(u, indices, index, color)
  color[u] = 1
  index = index + 1
  indices[u] = index

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit(v, indices, index, color)
    end
  end

  color[u] = 2
  return index
end

local function create_state_indices(u)
  local indices = {}
  visit(u, indices, 0, {})
  return indices
end

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

local function visit(u, map, indices)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if not transition.set then
      local v = transition.v
      map[indices[v]] = v
      visit(v, map, indices)
    end
  end
end

local function epsilon_closure(u, epsilon_closures, indices)
  local seq = epsilon_closures[u]
  if not seq then
    local map = { [indices[u]] = u }
    visit(u, map, indices)
    seq = map_to_seq(map)
    epsilon_closures[u] = seq
  end
  return seq
end

local function new_state(seq)
  local accept
  local timestamp
  for i = 1, #seq do
    local u = seq[i].state
    local a = u.accept
    if a then
      local t = assert(u.timestamp)
      if not timestamp or timestamp > t then
        accept = a
        timestamp = t
      end
    end
  end

  local state = fsm.new_state()
  state.accept = accept
  state.timestamp = timestamp
  seq.state = state
  return state
end

local function visit(useq, states, epsilon_closures, indices, color)
  local ukey = useq.key
  local unew = useq.state

  color[ukey] = 1

  local action_index = 0
  local actions = {}
  local new_transition_map = {}
  local new_states = {}

  for byte = 0x00, 0xFF do
    local vmap = {}
    local action
    local timestamp

    for i = 1, #useq do
      local transition = fsm.execute_transition(useq[i].state, byte)
      if transition then
        local t = transition.timestamp
        if not timestamp or timestamp > t then
          action = transition.action
          timestamp = t
        end
        local vseq = epsilon_closure(transition.v, epsilon_closures, indices)
        for j = 1, #vseq do
          local v = vseq[j]
          vmap[v.index] = v.state
        end
      end
    end

    if next(vmap) then
      local vseq = map_to_seq(vmap)
      local vkey = vseq.key
      local vnew

      local xseq = states[vkey]
      if not xseq then
        vnew = new_state(vseq)
        states[vkey] = vseq
        new_states[#new_states + 1] = vseq
      else
        vnew = xseq.state
      end

      local new_transition_key
      if action then
        -- actionの生の比較 (raw equality) を行うためにテーブルを経由する
        local index = actions[action]
        if not index then
          action_index = action_index + 1
          actions[action] = action_index
          new_transition_key = vkey .. ";" .. action_index
        else
          new_transition_key = vkey .. ";" .. index
        end
      else
        new_transition_key = vkey
      end

      local new_transition = new_transition_map[new_transition_key]
      if not new_transition then
        new_transition = fsm.new_transition(unew, vnew, { [byte] = true })
        new_transition.action = action
        new_transition.timestamp = timestamp
        new_transition_map[new_transition_key] = new_transition
      else
        new_transition.set[byte] = true
        assert(new_transition.action == action)
        if new_transition.timestamp > timestamp then
          new_transition.timestamp = timestamp
        end
      end
    end
  end

  for i = 1, #new_states do
    local vseq = new_states[i]
    if not color[vseq.key] then
      visit(vseq, states, epsilon_closures, indices, color)
    end
  end

  color[ukey] = 2
end

return function (u)
  local indices = create_state_indices(u)
  local epsilon_closures = {}
  local useq = epsilon_closure(u, epsilon_closures, indices)
  local unew = new_state(useq)
  visit(useq, { [useq.key] = useq }, epsilon_closures, indices, {})
  return unew
end
