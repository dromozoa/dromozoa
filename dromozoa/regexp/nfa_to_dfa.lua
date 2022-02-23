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

local function visit(u, state_indices, index, color)
  color[u] = 1
  index = index + 1
  state_indices[u] = index
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit(v, state_indices, index, color)
    end
  end
  color[u] = 2
  return index
end

local function create_state_indices(u)
  local state_indices = {}
  visit(u, state_indices, 0, {})
  return state_indices
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

local function visit(u, map, state_indices)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if not transition.set and not transition.enter and not transition.leave then
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

local function merge_priority(a, b)
  if b and (not a or a > b) then
    return b
  else
    return a
  end
end

local function visit(useq, new_states, epsilon_closures, state_indices, color)
  color[useq] = 1

  local new_transition_map = {}

  for byte = 0x00, 0xFF do
    local vmap = {}
    local min_timestamp
    local action

    for i = 1, #useq do
      local transitions = useq[i].state.transitions
      for j = 1, #transitions do
        local transition = transitions[j]
        local set = transition.set
        if set and set[byte] then
          local timestamp = assert(transition.timestamp)
          if not min_timestamp or min_timestamp > timestamp then
            min_timestamp = timestamp
            action = transition.action
          end
          local seq = epsilon_closure(transition.v, epsilon_closures, state_indices)
          for k = 1, #seq do
            local item = seq[k]
            vmap[item.index] = item.state
          end
        end
      end
    end

    if next(vmap) then
      local vseq = map_to_seq(vmap)
      local vkey = vseq.key
      local vobj = new_states[vkey]
      if not vobj then
        vobj = { state = fsm.new_state(), seq = vseq }
        new_states[vkey] = vobj
      end

      local new_transition_key = vkey
      if action then
        -- TODO タイムスタンプを代表値に使うのはどうか？
        new_transition_key = new_transition_key .. "/" .. action
      end

      local new_transition = new_transition_map[new_transition_key]
      if not new_transition then
        new_transition_map[new_transition_key] = { index = byte, v = vobj, set = { [byte] = true }, action = action, timestamp = min_timestamp }
      else
        new_transition.set[byte] = true
        -- TODO 単純に考えると同じアクションを持つ遷移はタイムスタンプも等しいはずであり、タイムスタンプを更新する必要はない
        -- TODO 同じアクションを複数回使っている場合はそれが満たされない場合もある、その場合はスプリットも検討するべき？
        if new_transition.timestamp > min_timestamp then
          new_transition.timestamp = min_timestamp
        end
      end
    end
  end

  -- leaveの処理
  local vmap = {}
  local min_timestamp
  local leave
  for i = 1, #useq do
    local transitions = useq[i].state.transitions
    for j = 1, #transitions do
      local transition = transitions[j]
      if transition.leave then
        local timestamp = assert(transition.timestamp)
        if not min_timestamp or min_timestamp > timestamp then
          min_timestamp = timestamp
          leave = transition.leave
        end
        local seq = epsilon_closure(transition.v, epsilon_closures, state_indices)
        for k = 1, #seq do
          local item = seq[k]
          vmap[item.index] = item.state
        end
      end
    end
  end

  if next(vmap) then
    local vseq = map_to_seq(vmap)
    local vkey = vseq.key
    local vobj = new_states[vkey]
    if not vobj then
      vobj = { state = fsm.new_state(), seq = vseq }
      new_states[vkey] = vobj
    end

    local new_transition_key = vkey
    if action then
      -- TODO タイムスタンプを代表値に使うのはどうか？
      new_transition_key = new_transition_key .. "/" .. action
    end

    local new_transition = { index = 256, v = vobj, leave = assert(leave), timestamp = min_timestamp }
    new_transition_map["leave"] = new_transition
  end

  local new_transitions = {}
  for _, new_transition in pairs(new_transition_map) do
    new_transitions[#new_transitions + 1] = new_transition
  end
  table.sort(new_transitions, function (a, b) return a.index < b.index end)

  local ukey = useq.key
  local uobj = new_states[ukey]
  local unew = uobj.state
  for i = 1, #new_transitions do
    local new_transition = new_transitions[i]

    local vobj = new_transition.v
    local vnew = vobj.state
    local vseq = vobj.seq

    if not new_transition.leave then
      local t = fsm.new_transition(unew, vnew, new_transition.set)
      t.action = new_transition.action
      t.timestamp = new_transition.timestamp
    else
      local t = fsm.new_transition(unew, vnew)
      t.leave = new_transition.leave
      t.timestamp = new_transition.timestamp
    end

    local accept
    for i = 1, #vseq do
      accept = merge_priority(accept, vseq[i].state.accept)
    end
    vnew.accept = accept

    if not color[vseq] then
      visit(vseq, new_states, epsilon_closures, state_indices, color)
    end
  end

  color[useq] = 2
end

return function (u)
  local state_indices = create_state_indices(u)
  local epsilon_closures = {}
  local useq = epsilon_closure(u, epsilon_closures, state_indices)
  local unew = fsm.new_state()
  local new_states = { [useq.key] = { state = unew, seq = useq } }
  visit(useq, new_states, epsilon_closures, state_indices, {})
  return unew
end
