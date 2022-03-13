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

local function visit1(def, u, state_indices, state_index, transition_indices, transition_index, color)
  color[u] = 1

  local accept = u.accept
  if accept then
    state_index = state_index + 1
    state_indices[u] = state_index
    def.accept_actions[state_index] = u.accept
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
      state_index, transition_index = visit1(def, v, state_indices, state_index, transition_indices, transition_index, color)
    end
  end

  color[u] = 2
  return state_index, transition_index
end

local function visit2(def, u, state_indices, state_index, transition_indices, color)
  color[u] = 1

  local x = state_indices[u]
  if not x then
    state_index = state_index + 1
    state_indices[u] = state_index
    x = state_index
  end

  local transition_to_states = def.transition_to_states
  local transition_actions = def.transition_actions

  local transitions = u.transitions
  for i = 1, #transitions  do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      state_index = visit2(def, v, state_indices, state_index, transition_indices, color)
    end
    local action = transition.action
    if action then
      local y = transition_indices[transition]
      transition_to_states[y] = state_indices[transition.v]
      transition_actions[y] = action
    end
  end

  local transitions = def.transitions

  for byte = 0x00, 0xFF do
    local transition = fsm.execute_transition(u, byte)
    if transition then
      if transition.action then
        transitions[byte][x] = -transition_indices[transition]
      else
        transitions[byte][x] = assert(state_indices[transition.v])
      end
    else
      transitions[byte][x] = 0
    end
  end

  color[u] = 2
  return state_index
end

return function (source)
  local definitions = {}

  for name, u in pairs(source) do
    local transitions = {}
    for byte = 0x00, 0xFF do
      transitions[byte] = {}
    end

    local def = {
      name = name;
      accept_actions = {};
      transition_to_states = {};
      transition_actions = {};
      transitions = transitions;
    }

    local state_indices = {}
    local transition_indices = {}
    local max_accept_state, max_transition =  visit1(def, u, state_indices, 0, transition_indices, 0, {})
    def.max_accept_state = max_accept_state
    def.max_transition = max_transition
    local max_state = visit2(def, u, state_indices, max_accept_state, transition_indices, {})
    def.start_state = state_indices[u]
    def.max_state = max_state

    definitions[#definitions + 1] = { timestamp = u.timestamp, def = def }
  end
  table.sort(definitions, function (a, b) return a.timestamp < b.timestamp end)

  local result = {}
  for i = 1, #definitions do
    result[i] = definitions[i].def
  end
  return result
end
