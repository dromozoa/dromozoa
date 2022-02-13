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

local class = {}

function class.new_state()
  return { transitions = {} }
end

function class.new_transition(u, v, set)
  local transition = { v = v, set = set }
  local transitions = u.transitions
  transitions[#transitions + 1] = transition
  return transition
end

local function visit1(u, state_to_index, index_to_state, index, color)
  color[u] = 1
  if u.accept then
    index = index + 1
    state_to_index[u] = index
    index_to_state[index] = u
  else
    local transitions = u.transitions
    for i = 1, #transitions do
      local transition = transitions[i]
      local v = transition.v
      if not color[v] then
        index = visit1(v, state_to_index, index_to_state, index, color)
      end
    end
  end
  return index
end

local function visit2(u, state_to_index, index_to_state, index, color)
  color[u] = 1
  if not u.accept then
    index = index + 1
    state_to_index[u] = index
    index_to_state[index] = u

    local transitions = u.transitions
    for i = 1, #transitions do
      local transition = transitions[i]
      local v = transition.v
      if not color[v] then
        index = visit2(v, state_to_index, index_to_state, index, color)
      end
    end
  end
  return index
end

function class.create_state_indices(u)
  local state_to_index = {}
  local index_to_state = {}
  local max_accept_index = visit1(u, state_to_index, index_to_state, 0, {})
  visit2(u, state_to_index, index_to_state, max_accept_index, {})
  return state_to_index, index_to_state, max_accept_index
end

return class
