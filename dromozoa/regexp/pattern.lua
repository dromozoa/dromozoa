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
local metatable = { __index = class }

local timestamp = 0

local function construct(...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, ... }, metatable)
end

local function concat(items)
  local result = items[1]
  for i = 2, #items do
    result = result * items[i]
  end
  return result
end

local any = {}
for byte = 0x00, 0xFF do
  any[byte] = true
end

local function pattern(that)
  local t = type(that)
  if t == "number" then
    local items = {}
    for i = 1, that do
      items[i] = construct("[", any)
    end
    return concat(items)
  elseif t == "string" then
    local items = {}
    for i = 1, #that do
      items[i] = construct("[", { [that:byte(i)] = true })
    end
    return concat(items)
  else
    return that
  end
end

class.pattern = pattern

function class.set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return construct("[", set)
end

function class.range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for j = a, b do
      set[j] = true
    end
  end
  return construct("[", set)
end

function metatable:__pow(that)
  if self[1] == "%" then
    error "not supported"
  end
  if that < 0 then
    local items = {}
    for i = 1, -that do
      items[i] = construct("?", self)
    end
    return concat(items)
  elseif that == 0 then
    return construct("*", self)
  elseif that == 1 then
    return construct("+", self)
  else
    local items = {}
    for i = 1, that - 1 do
      items[i] = self
    end
    items[that] = construct("+", self)
    return concat(items)
  end
end

function metatable:__mul(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[1] == "%" or that[1] == "%" then
    error "not supported"
  end
  return construct(".", self, that)
end

function metatable:__add(that)
  local self = pattern(self)
  local that = pattern(that)
  local self_op = self[1]
  local that_op = that[1]
  if self_op == "[" and that_op == "[" then
    local set = {}
    for byte in pairs(self[2]) do
      set[byte] = true
    end
    for byte in pairs(that[2]) do
      set[byte] = true
    end
    return construct("[", set)
  else
    if self_op == "%" or that_op == "%" then
      error "not supported"
    end
    return construct("|", self, that)
  end
end

function metatable:__unm(that)
  if self[1] == "[" then
    local set = self[2]
    local neg = {}
    for byte = 0x00, 0xFF do
      if not set[byte] then
        neg[byte] = true
      end
    end
    return construct("[", neg)
  else
    error "not supported"
  end
end

function metatable:__sub(that)
  return construct("-", self, that)
end

function metatable:__div(action)
  if self[1] == "[" then
    return construct("/", self, action)
  else
    error "not supported"
  end
end

function metatable:__mod(action)
  return construct("%", self, action)
end

return class
