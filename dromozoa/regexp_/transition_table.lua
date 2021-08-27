-- Copyright (C) 2021 Tomoyuki Fujimori <moyu@dromozoa.com>
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
local metatable = { __index = class }

function class.new(n)
  local self = { max_state = 0 }
  for ev = 0, 255 + n do
    self[ev] = {}
  end
  return setmetatable(self, metatable)
end

function class:new_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:set_transition(u, v, ev)
  self[ev][u] = v
end

function class:set_transitions(u, v, set)
  for ev in pairs(set) do
    self[ev][u] = v
  end
end

function class:set_epsilon_transition(u, v)
  for ev = 256, #self do
    if not self[ev][u] then
      self[ev][u] = v
      return
    end
  end
  error "out of range"
end

return class
