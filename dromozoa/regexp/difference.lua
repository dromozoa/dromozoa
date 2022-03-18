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
    return fsm.execute_transition(u, byte)
  end
end

return function (this, that)
  local this_states, this_indices = create_states_and_indices(this)
  local that_states, that_indices = create_states_and_indices(that)

  local n = #this_states + 1

  local new_states = {}

  for i = 0, #this_states do
    local ux = this_states[i]

    for j = 0, #that_states do
      local uy = that_states[j]

      -- ux, uy
      -- x, y
      -- vx, vy
      -- ukey, unew
      -- vkey, vnew

      local ukey = i + j * n
      if ukey ~= 0 then

        local action_index = 0
        local actions = {}
        local new_transition_map = {}

        for byte = 0x00, 0xFF do
          local x = execute_transition(ux, byte)
          local y = execute_transition(uy, byte)
          local vx
          local vy
          local vkey = 0
          if x then
            vx = x.v
            vkey = this_indices[vx]
          end
          if y then
            vy = y.v
            vkey = vkey + that_indices[vy] * n
          end
          if vkey ~= 0 then
            local unew = new_states[ukey]
            if not unew then
              unew = fsm.new_state()
              new_states[ukey] = unew

              if ux and ux.accept then
                if not uy or not uy.accept then
                  unew.accept = ux.accept
                  unew.timestamp = ux.timestamp
                end
              end
            end
            local vnew = new_states[vkey]
            if not vnew then
              vnew = fsm.new_state()
              new_states[vkey] = vnew

              if vx and vx.accept then
                if not vy or not vy.accept then
                  vnew.accept = vx.accept
                  vnew.timestamp = vx.timestamp
                end
              end
            end

            local action
            local timestamp
            local new_transition_key
            if x then
              action = x.action
              timestamp = x.timestamp
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
            else
              new_transition_key = vkey
            end
            if y then
              if not timestamp then
                timestamp = y.timestamp
              end
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

  local uid = this_indices[this] + n * that_indices[that]
  local u = new_states[uid]
  u.timestamp = this.timestamp

  -- remove dead states
  remove_dead_states(u)

  return u
end
