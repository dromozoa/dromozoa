-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

---@class dromozoa.token
---@field kind string
---@field file string?
---@field line integer
---@field column integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.token",
}

---@param kind string
---@return dromozoa.token
function class.new(kind)
  return setmetatable({
    kind = kind,
    filename = nil,
    line = 0,
    column = 0,
  }, metatable)
end

return class
