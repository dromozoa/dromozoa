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

---@type fun():integer, boolean
local f = function()
  return 42, true
end

---@type integer, boolean
local x, y = f()
print(x, y)

---@class クラス
---@field value integer
local class = {}
local metatable = { __index = class }

---@param value integer
---@return クラス
function class.new(value)
  return setmetatable({ value = value }, metatable)
end

---@alias _-.* クラス

local x = class.new(42)
print(x.value)

---@type _-.*
local y
y = x
print(y.value)
