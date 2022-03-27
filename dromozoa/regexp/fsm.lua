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

local private = function () end

function class.new_state()
  return { transitions = {} }
end

function class.new_transition(u, v, set, action)
  local transition = { v = v, set = set, action = action }
  local transitions = u.transitions
  transitions[#transitions + 1] = transition
  return transition
end

function class.execute_transition(u, byte)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local set = transition.set
    if set and set[byte] then
      return transition
    end
  end
end

function class.merge_timestamp(result, timestamp)
  if not result or result > timestamp then
    return timestamp
  else
    return result
  end
end

function class.new_transition_key(key, actions, action)
  if action then
    local index = actions[action]
    if not index then
      index = (actions[private] or 0) + 1
      actions[action] = index
      actions[private] = index
    end
    return key .. ";" .. index
  else
    return tostring(key)
  end
end

return class
