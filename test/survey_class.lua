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

---@class my_optional_integer
---@field private valid boolean
---@field private value integer
local class = {}
local metatable = { __index = class }

local function new_invalid_optional_integer()
  return setmetatable({ valid = false, value = 0 }, metatable)
end

---@param value integer
---@return my_optional_integer
local function new_optional_integer(value)
  return setmetatable({ valid = true, value = value }, metatable)
end

---@return boolean
function class:is_valid()
  return self.valid
end

---@return integer
function class:get_value()
  assert(self:is_valid())
  return self.value
end

local my_optional_integer = class

---@class my_optional_boolean
---@field private value integer
local class = {}
local metatable = { __index = class }

local function new_invalid_optional_boolean()
  return setmetatable({ value = -1 }, metatable)
end

---@param value boolean
---@return my_optional_boolean
local function new_optional_boolean(value)
  return setmetatable({ value = value and 1 or 0 }, metatable)
end

---@return boolean
function class:is_valid()
  return self.value ~= -1
end

---@return boolean
function class:get_value()
  assert(self:is_valid())
  return self.value > 0
end

local my_optional_boolean = class

local a = new_invalid_optional_integer()
local b = new_invalid_optional_boolean()
local c = new_optional_integer(42)
local d = new_optional_boolean(false)

print(a:is_valid(), b:is_valid(), c:is_valid(), d:is_valid())
print(my_optional_integer)
print(my_optional_boolean)

---@type fun():integer, boolean
local f = function()
  return 42, true
end

---@type integer, boolean
local x, y = f()
print(x, y)
