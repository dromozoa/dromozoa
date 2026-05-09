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

---@class my.awesome.class
---@field x number
---@field y number
---@operator add(my.awesome.class): my.awesome.class
local class = {}

---@private
class.__index = class

---@param that my.awesome.class
function class:__add(that)
  return class.new(self.x + that.x, self.y + that.y)
end

---@param x number?
---@param y number?
---@return my.awesome.class
function class.new(x, y)
  return setmetatable({
    x = x or 0,
    y = y or 0,
  }, class)
end

---@param v integer
function class:f(v)
  print(v)
end

---@return number
function class:d()
  return math.sqrt(self.x * self.x + self.y * self.y)
end

local u = class.new(2, 3)
local v = class.new(3, 4)
u:f(42)
local u = u + v
print(u:d())
