-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local function new(code, a, b)
  return setmetatable({ [0] = code, a, b }, metatable)
end

local any = {}
for byte = 0x00, 0xFF do
  any[byte] = true
end
local any = new("[", any)

local function pattern(that)
  local t = type(that)
  if t == "number" then
    local result = any
    for i = 2, that do
      result = new(".", result, any)
    end
    return result
  elseif t == "string" then
    local result = new("[", { [that:byte()] = true })
    for i = 2, #that do
      result = new(".", result, new("[", { [that:byte(i)] = true }))
    end
    return result
  else
    return that
  end
end

class.pattern = pattern

function class.range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for byte = a, b do
      set[byte] = true
    end
  end
  return new("[", set)
end

function class.set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return new("[", set)
end

function metatable:__add(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "[" and that[0] == "[" then
    local set = {}
    for byte in pairs(self[1]) do
      set[byte] = true
    end
    for byte in pairs(that[1]) do
      set[byte] = true
    end
    return new("[", set)
  else
    return new("|", self, that)
  end
end

function metatable:__mul(that)
  local self = pattern(self)
  local that = pattern(that)
  return new(".", self, that)
end

function metatable:__pow(that)
  local t = type(that)
  if t == "table" then
    local m = that[1]
    local n = that[2]
    if not n then
      n = m
    end
    if m == 0 then
      local result = new("?", self)
      for i = 2, n do
        result = new(".", result, new("?", self))
      end
      return result
    else
      local result = self
      for i = 2, m do
        result = new(".", result, self)
      end
      for i = m + 1, n do
        result = new(".", result, new("?", self))
      end
      return result
    end
  else
    if t == "string" then
      if that == "*" then
        that = 0
      elseif that == "+" then
        that = 1
      elseif that == "?" then
        that = -1
      end
    end

    if that == 0 then
      return new("*", self)
    elseif that > 0 then
      local result = self
      for i = 2, that do
        result = new(".", result, self)
      end
      return new(".", result, new("*", self))
    else
      local result = new("?", self)
      for i = 2, -that do
        result = new(".", result, new("?", self))
      end
      return result
    end
  end
end

function metatable:__unm()
  if self[0] == "[" then
    local set = self[1]
    local neg = {}
    for byte = 0x00, 0xFF do
      if not set[byte] then
        neg[byte] = true
      end
    end
    return new("[", neg)
  else
    error "negative lookahead not supported"
  end
end

return class
