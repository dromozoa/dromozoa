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
  for i = 1, select("#", ...) do
    local v = select(i, ...)
    if v == nil then
      error("bad argument #" .. i .. " (value expected)")
    end
    self[#self + 1] = v
  end
  return self
end

function class:slice(m, n)
  if m == nil then
    m = 1
  end
  if n == nil then
    n = #self
  end

  local result = class()
  for i = m, n do
    local v = self[i]
    if v == nil then
      error("bad field #" .. i .. " (value expected)")
    end
    result[#result + 1] = self[i]
  end
  return result
end

return setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({}, metatable):append(...)
  end
})
