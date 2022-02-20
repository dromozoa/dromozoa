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

local function clone(self)
  if getmetatable(self) == metatable then
    local that = {}
    for k, v in pairs(self) do
      that[k] = clone(v)
    end
    return setmetatable(that, metatable)
  else
    return self
  end
end

local function concat(items)
  local result = items[1]
  for i = 2, #items do
    result = result * items[i]
  end
  return result
end

local any_set = {}
for ev = 0, 255 do
  any_set[ev] = true
end
local any = construct("[", any_set)

local function pattern(that)
  local t = type(that)
  if t == "number" then
    local items = {}
    for i = 1, that do
      items[i] = clone(any)
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
  if that < 0 then
    local items = { construct("?", self) }
    for i = 2, -that do
      items[i] = construct("?", clone(self))
    end
    return concat(items)
  elseif that == 0 then
    return construct("*", self)
  else
    local items = { self }
    for i = 2, that do
      items[i] = clone(self)
    end
    items[that + 1] = construct("*", clone(self))
    return concat(items)
  end
end

function metatable:__mul(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
  return construct(".", self, that)
end

function metatable:__add(that)
  local self = class.pattern(self)
  local that = class.pattern(that)
  return construct("|", self, that)
end

function metatable:__unm(that)
  if self[1] == "[" then
    local set = self[2]
    local neg = {}
    for ev = 0, 255 do
      if not set[ev] then
        neg[ev] = true
      end
    end
    return construct("[", neg)
  else
    error "not supported"
  end
end

function metatable:__div(that)
  if self[1] == "[" then
    return construct("/", self, that)
  else
    error "not supported"
  end
end

function metatable:__mod(that)
  local self = class.pattern(self)
  return construct("%", self, that)
end

return class
