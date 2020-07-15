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

local any = {}
for byte = 0x00, 0xFF do
  any[byte] = true
end

local class = {}
local metatable = { __index = class }

local function new(name, ...)
  return setmetatable({ [0] = name, ... }, metatable)
end

local function clone(self)
  local name = self[0]
  if name == "[" then
    return new(name, self[1])
  else
    local that = { [0] = name }
    for i = 1, #self do
      that[i] = clone(self[i])
    end
    return setmetatable(that, metatable)
  end
end

function class.pattern(that)
  local t = type(that)
  if t == "number" then
    local result = new("[", any)
    for i = 2, that do
      result = result * new("[", any)
    end
    return result
  elseif t == "string" then
    local result = new("[", { [that:byte(1)] = true })
    for i = 2, #that do
      result = result * new("[", { [that:byte(i)] = true })
    end
    return result
  else
    return that
  end
end

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

function class:clone()

  local code = self[0]
end

function metatable:__add(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
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
  local self = class.pattern(self)
  local that = class.pattern(that)
  return new(".", self, that)
end

function metatable:__pow(that)
  if that == 0 or that == "*" then
    return new("*", self)
  elseif that == 1 or that == "+" then
    return self * clone(self)^0
  elseif that == -1 or that == "?" then
    return new("?", self)
  else
    if type(that) == "number" then
      if that < 0 then
        local result = self^-1
        for i = 2, -that do
          result = result * clone(self)^-1
        end
        return result
      else
        local result = self
        for i = 2, that do
          result = result * clone(self)
        end
        return result * clone(self)^0
      end
    else

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
  end
end

return class
