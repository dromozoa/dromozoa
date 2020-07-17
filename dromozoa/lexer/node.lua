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

function class.new(code, a, b)
  return setmetatable({ [0] = code, a, b }, metatable)
end

local any = {}
for byte = 0x00, 0xFF do
  any[byte] = true
end
class.any = class.new("[", any)

function class.pattern(that)
  local t = type(that)
  if t == "number" then
    local result = class.any
    for i = 2, that do
      result = result * class.any
    end
    return result
  elseif t == "string" then
    local result = class.new("[", { [that:byte()] = true })
    for i = 2, #that do
      result = result * class.new("[", { [that:byte(i)] = true })
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
  return class.new("[", set)
end

function class.set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return class.new("[", set)
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
    return class.new("[", set)
  else
    return class.new("|", self, that)
  end
end

function metatable:__mul(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
  return class.new(".", self, that)
end

function metatable:__pow(that)
  if that == 0 or that == "*" then
    return class.new("*", self)
  elseif that == 1 or that == "+" then
    return self * self^0
  elseif that == -1 or that == "?" then
    return class.new("?", self)
  else
    if type(that) == "number" then
      if that < 0 then
        local result = self^-1
        for i = 2, -that do
          result = result * self^-1
        end
        return result
      else
        local result = self
        for i = 2, that do
          result = result * self
        end
        return result * self^0
      end
    else
      local min = self[1]
      local max = self[2]
      if not max then
        max = min
      end
      if min == 0 then -- {0,3}
        return self^-max
      else
        local result = self
        for i = 2, min do
          result = result * self
        end
        for i = min + 1, max do
          result = result * self^-1
        end
        return result
      end
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
    return class.new("[", neg)
  end
end

return class
