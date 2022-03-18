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
local remove_dead_states = require "dromozoa.regexp.remove_dead_states"

local function visit(u, states, indices, index, color)
  color[u] = 1
  index = index + 1
  indices[u] = index
  states[index] = u

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit(v, states, indices, index, color)
    end
  end

  color[u] = 2
  return index
end

local function create_states_and_indices(u)
  local states = {}
  local indices = {}
  visit(u, states, indices, 0, {})
  return states, indices
end

local function execute_transition(u, byte)
  if u then
    local transition = fsm.execute_transition(u, byte)
    if transition then
      return transition, transition.v, transition.action
    end
  end
end

local function new_state(ux, uy)
  local state = fsm.new_state()
  if ux then
    local accept = ux.accept
    if accept and (not uy or not uy.accept) then
      state.accept = accept
      state.timestamp = ux.timestamp
    end
  end
  return state
end

return function (ux, uy)
  local x_states, x_indices = create_states_and_indices(ux)
  local y_states, y_indices = create_states_and_indices(uy)

  local nx = #x_states
  local ny = #y_states
  local n = nx + 1

  local new_states = {}

  for i = 0, nx do
    local ux = x_states[i]
    for j = 0, ny do
      local uy = y_states[j]
      local ukey = i + j * n
      if ukey ~= 0 then
        local action_index = 0
        local actions = {}
        local new_transition_map = {}

        for byte = 0x00, 0xFF do
          local tx, vx, action = execute_transition(ux, byte)
          local ty, vy = execute_transition(uy, byte)

          local vkey = 0
          local timestamp
          if ty then
            vkey = y_indices[vy] * n
            timestamp = ty.timestamp
          end
          if tx then
            vkey = x_indices[vx] + vkey
            timestamp = tx.timestamp
          end

          if vkey ~= 0 then
            local unew = new_states[ukey]
            if not unew then
              unew = new_state(ux, uy)
              new_states[ukey] = unew
            end
            local vnew = new_states[vkey]
            if not vnew then
              vnew = new_state(vx, vy)
              new_states[vkey] = vnew
            end

            local new_transition_key
            if action then
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
              if new_transition.timestamp > timestamp then
                new_transition.timestamp = timestamp
              end
            end
          end
        end
      end
    end
  end

  local unew = new_states[x_indices[ux] + y_indices[uy] * n]
  unew.timestamp = ux.timestamp
  return remove_dead_states(unew)
end
