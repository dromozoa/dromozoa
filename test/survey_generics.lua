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

---@class my_class<T>
local class = {}
local metatable = { __index = class }

---@generic T
---@param x T
---@return my_class<T>
function class.new1(x)
  return setmetatable({ x }, metatable)
end

---@generic T
---@param type `T`
---@return my_class<T>
function class.new2(type )
  return setmetatable({ type = type }, metatable)
end

---@param self my_class<T>
---@return T
function class.front1(self)
  return self[1]
end

---@return T
function class:front2()
  return self[1]
end

---@param v T
function class:push(v)
  table.insert(self, v)
end

-- local u = class.new(math.pi // 1)
local u = class.new1(_VERSION:rep(1))
local x = u
local y = u:front1()
local z = u:front2()
print(x, y, z)

local u = class.new2(_VERSION:rep(1))
-- local u = class.new2 "integer"
local x = u
local y = u:front1()
local z = u:front2()
u:push(true)
print(x, y, z)
