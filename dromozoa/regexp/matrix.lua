-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

function class.new()
  local self = { max_state = 0 }
  for i = 0, 257 do
    self[i] = {}
  end
  return setmetatable(self, metatable)
end

function class:add_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:add_transition(u, v, set)
  for i in pairs(set) do
    self[i][u] = v
  end
end

function class:add_epsilon_transition(u, v)
  for i = 256, #self do
    if not self[u] then
      self[u] = v
      return
    end
  end
end

return class
