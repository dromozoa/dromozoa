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

local function visit1(u, not_dead_states, color)
  color[u] = 1

  if u.accept then
    not_dead_states[u] = true
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit1(v, not_dead_states, color)
    end
    if not_dead_states[v] then
      not_dead_states[u] = true
    end
  end

  color[u] = 2
end

local function visit2(u, not_dead_states, color)
  color[u] = 1

  local transitions = u.transitions
  local new_transitions = {}
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit2(v, not_dead_states, color)
    end
    if not_dead_states[v] then
      new_transitions[#new_transitions + 1] = transition
    end
  end
  u.transitions = new_transitions

  color[u] = 2
end

return function (u)
  local not_dead_states = {}
  visit1(u, not_dead_states, {})
  visit2(u, not_dead_states, {})
  return u
end
