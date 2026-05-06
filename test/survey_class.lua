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

---@class my_i32
---@field value integer
---@operator add(my_i32): my_i32
local class = {}
local metatable = { __index = class }

---@param that my_i32
---@return my_i32
function metatable:__add(that)
  return class.new(self.value + that.value)
end

---@param value integer
function class.new(value)
  return setmetatable({
    value = value,
  }, metatable)
end

---@return string
function class:to_hex_string()
  return ("0x%X"):format(self.value)
end

---@return string
function class:to_string()
  return ("%d"):format(self.value)
end

local u = class.new(-17)
print(u:to_hex_string(), u:to_string())

local x = u + u
print(x:to_hex_string(), x:to_string())

local super = class

---@class my_u32: my_i32
---@operator add(my_u32): my_u32
local class = {}
local metatable = {}

function metatable:__index(index)
  return class[index] or super[index]
end

---@param that my_u32
---@return my_u32
function metatable:__add(that)
  return class.new(self.value + that.value)
end

---@param value integer
---@return my_u32
function class.new(value)
  return setmetatable({
    value = value,
  }, metatable)
end

---@return string
function class:to_string()
  return ("%u"):format(self.value)
end

local v = class.new(-23)
print(v:to_hex_string(), v:to_string())

local y = v + v
print(y:to_hex_string(), y:to_string())

-- my_u32がmy_i32にアップキャストされて演算子が解決されるらしい。
local z = y + x
print(z:to_string())
