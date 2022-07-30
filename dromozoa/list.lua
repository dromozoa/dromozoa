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
local metatable = { __index = class, __name = "dromozoa.list" }

function class:append(...)
  local n = #self
  for i = 1, select("#", ...) do
    n = n + 1
    self[n] = select(i, ...)
  end
  return self
end

function class:slice(i, j)
  local that = class()
  local n = 0
  for i = i == nil and 1 or i, j == nil and #self or j do
    n = n + 1
    that[n] = self[i]
  end
  return that
end

return setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({}, metatable):append(...)
  end
})
