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
local table_unpack = table.unpack or unpack

-- TODO シークエンスを強制したい

function class:append(...)
  local n = #self
  for i = 1, select("#", ...) do
    local v = select(i, ...)
    if v == nil then
      error("bad argument #" .. i .. " (value expected)")
    end
    self[n + i] = v
  end
  return self
end

function class:unpack(m, n)
  return table_unpack(self, m, n)
end

function class:slice(m, n)
  return setmetatable({}, metatable):append(self:unpack(m, n))
end

function class:ipairs()
  return ipairs(self)
end

return setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({}, metatable):append(...)
  end
})
