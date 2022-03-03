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

local function merge_accept(seq)
  local result
  for i = 1, #seq do
    local accept = seq[i].state.accept
    if accept and (not result or result > accept) then
      result = accept
    end
  end
  return result
end

local function visit(useq, new_states, epsilon_closures, indices, color)
  color[useq] = 1

  local ukey = useq.key
  local uobj = new_states[ukey]
  local unew = uobj.state

  local new_transition_map = {}

  for byte = 0x00, 0xFF do
    local vmap = {}
    local timestamp
    local action

    for i = 1, #useq do
      local transition = fsm.execute_transition(useq[i].state, byte)
      if transition then
        local t = transition.timestamp
        if not timestamp or timestamp > t then
          timestamp = t
          action = transition.action
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
      local v = new_states[vkey]
      if not v then
        local vnew = fsm.new_state()
        vnew.accept = merge_accept(vseq)
        v = { state = vnew, seq = vseq }
        new_states[vkey] = v
      end

      local new_transition_key = vkey .. ";" .. timestamp
      if action then
        new_transition_key = new_transition_key .. ";" .. action
      end

      local new_transition = new_transition_map[new_transition_key]
      if not new_transition then
        new_transition = fsm.new_transition(unew, v.state, { [byte] = true })
        new_transition.index = byte
        new_transition.vseq = v.seq
        new_transition.action = action
        new_transition.timestamp = timestamp
        new_transition_map[new_transition_key] = new_transition
        -- { index = byte, v = v, set = { [byte] = true }, timestamp = timestamp, action = action }
      else
        new_transition.set[byte] = true
      end
    end
  end

  local new_transitions = {}
  for _, new_transition in pairs(new_transition_map) do
    new_transitions[#new_transitions + 1] = new_transition
  end
  table.sort(new_transitions, function (a, b) return a.index < b.index end)

  for i = 1, #new_transitions do
    local new_transition = new_transitions[i]

    -- local v = new_transition.v
    -- local vnew = v.state
    local vseq = new_transition.vseq

    -- local t = fsm.new_transition(unew, vnew, new_transition.set)
    -- t.action = new_transition.action
    -- t.timestamp = new_transition.timestamp

    if not color[vseq] then
      visit(vseq, new_states, epsilon_closures, indices, color)
    end
  end

  color[useq] = 2
end

return function (u)
  local indices = create_state_indices(u)
  local epsilon_closures = {}
  local useq = epsilon_closure(u, epsilon_closures, indices)
  local unew = fsm.new_state()
  unew.accept = merge_accept(useq)
  -- TODO new_statesというのはカッコ悪い気がする
  local new_states = { [useq.key] = { state = unew, seq = useq } }
  visit(useq, new_states, epsilon_closures, indices, {})
  return unew
end
