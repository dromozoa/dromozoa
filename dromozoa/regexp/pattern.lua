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

function class.new(...)
  return setmetatable({...}, metatable)
end

function class:clone()
  local code = self[1]
  if code == "[" then
    return class.new("[", self[2])
  else
    local that = { code }
    for i = 2, #that do
      that[i] = self[i]:clone()
    end
    return setmetatable(that, metatable)
  end
end

function class.concat(items)
  local result = items[1]
  for i = 2, #items do
    result = result * items[i]
  end
  return result
end

local set = {}
for byte = 0x00, 0xFF do
  set[byte] = true
end
class.any = class.new("[", set)

function class.pattern(that)
  local t = type(that)
  if t == "number" then
    local items = {}
    for i = 1, that do
      items[i] = class.any:clone()
    end
    return class.concat(items)
  elseif t == "string" then
    local items = {}
    for i = 1, #that do
      items[i] = class.new("[", { [that:byte(i)] = true })
    end
    return class.concat(items)
  else
    return that
  end
end

function class.set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return class.new("[", set)
end

function class.range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for j = a, b do
      set[j] = true
    end
  end
  return class.new("[", set)
end

function metatable:__pow(that)
  if that < 0 then
    local items = { class.new("?", self) }
    for i = 2, -that do
      items[i] = class.new("?", self:clone())
    end
    return class.concat(items)
  elseif that == 0 then
    return class.new("*", self)
  else
    local items = { self }
    for i = 2, that do
      items[i] = self:clone()
    end
    items[that + 1] = class.new("*", self:clone())
    return class.concat(items)
  end
end

function metatable:__mul(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
  return class.new(".", self, that)
end

function metatable:__add(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
  return class.new("|", self, that)
end

function metatable:__unm(that)
  local self = class.pattern(self)
  if self[1] == "[" then
    local set = self[2]
    local neg = {}
    for byte = 0x00, 0xFF do
      if not set[byte] then
        neg[byte] = true
      end
    end
    return class.new("[", neg)
  else
    error "negative lookahead not supported"
  end
end

return class
