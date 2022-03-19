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

local fsm = require "dromozoa.regexp.fsm"

local function visit1(u, state_indices, state_index, transition_indices, transition_index, color)
  color[u] = 1

  if u.accept_action then
    state_index = state_index + 1
    state_indices[u] = state_index
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if transition.action then
      transition_index = transition_index + 1
      transition_indices[transition] = transition_index
    end
    local v = transition.v
    if not color[v] then
      state_index, transition_index = visit1(v, state_indices, state_index, transition_indices, transition_index, color)
    end
  end

  color[u] = 2
  return state_index, transition_index
end

local function visit2(u, state_indices, state_index, transition_indices, color)
  color[u] = 1

  local index = state_indices[u]
  if not index then
    state_index = state_index + 1
    state_indices[u] = state_index
    index = state_index
  end

  local transitions = u.transitions
  for i = 1, #transitions  do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      state_index = visit2(v, state_indices, state_index, transition_indices, color)
    end
  end

  color[u] = 2
  return state_index
end

local function visit3(def, u, state_indices, transition_indices, color)
  color[u] = 1

  local index = state_indices[u]
  local accept_action = u.accept_action
  if accept_action then
    def.accept_actions[index] = accept_action
  end

  local transition_to_states = def.transition_to_states
  local transition_actions = def.transition_actions

  local transitions = u.transitions
  for i = 1, #transitions  do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      visit3(def, v, state_indices, transition_indices, color)
    end
    local transition_action = transition.action
    if transition_action then
      local index = transition_indices[transition]
      transition_to_states[index] = state_indices[transition.v]
      transition_actions[index] = transition_action
    end
  end

  local max_state = def.max_state
  local transitions = def.transitions

  for byte = 0x00, 0xFF do
    local transition = fsm.execute_transition(u, byte)
    if transition then
      if transition.action then
        transitions[byte][index] = max_state + transition_indices[transition]
      else
        transitions[byte][index] = state_indices[transition.v]
      end
    else
      transitions[byte][index] = 0
    end
  end

  color[u] = 2
end

return function (data)
  local definitions = {}

  for name, u in pairs(data) do
    local state_indices = {}
    local transition_indices = {}
    local max_accept_state, max_transition =  visit1(u, state_indices, 0, transition_indices, 0, {})
    local max_state = visit2(u, state_indices, max_accept_state, transition_indices, {})

    local transitions = {}
    for byte = 0x00, 0xFF do
      transitions[byte] = {}
    end
    local def = {
      name = name;
      loop = u.loop;
      guard_action = u.guard_action;
      max_accept_state = max_accept_state;
      accept_actions = {};
      max_transition = max_transition;
      transition_to_states = {};
      transition_actions = {};
      start_state = state_indices[u];
      max_state = max_state;
      transitions = transitions;
      token_names = u.token_names;
    }
    visit3(def, u, state_indices, transition_indices, {})

    definitions[#definitions + 1] = { timestamp = u.timestamp, def = def }
  end
  table.sort(definitions, function (a, b) return a.timestamp < b.timestamp end)

  local result = {}
  for i = 1, #definitions do
    result[i] = definitions[i].def
  end
  return result
end
