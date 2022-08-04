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

local metatable = { __name = "dromozoa.regexp.pattern" }

local any = {}
for byte = 0x00, 0xFF do
  any[byte] = true
end
local module = setmetatable({ [0] = "[", any }, metatable)

local timestamp = 0

local function construct(code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

local function pattern(that)
  if type(that) == "string" then
    local self = construct("[", { [that:byte(1)] = true })
    for i = 2, #that do
      self = self + construct("[", { [that:byte(i)] = true })
    end
    return rawset(self, "literal", that)
  elseif that == module then
    return construct("[", module[1])
  else
    assert(getmetatable(that) == metatable)
    assert(that.timestamp ~= nil)
    return that
  end
end

local function range(that)
  if type(that) == "string" then
    local set = {}
    for i = 1, #that, 2 do
      local a, b = that:byte(i, i + 1)
      if b == nil then
        b = a
      end
      for byte = a, b do
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    return pattern(that)
  end
end

local function set(that)
  if type(that) == "string" then
    local set = {}
    for i = 1, #that do
      set[that:byte(i)] = true
    end
    return construct("[", set)
  else
    return pattern(that)
  end
end

local function union(self, that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  elseif self[0] == "[" and that[0] == "[" then
    local set = {}
    for byte in pairs(self[1]) do
      set[byte] = true
    end
    for byte in pairs(that[1]) do
      set[byte] = true
    end
    return construct("[", set)
  else
    return construct("|", self, that)
  end
end

function metatable:__add(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  else
    return construct(".", self, that)
  end
end

function metatable:__mul(that)
  local self = pattern(self)
  if self[0] == "%" then
    error "not supported"
  else
    local m
    local n

    if that == "*" then
      m = 0
    elseif that == "+" then
      m = 1
    elseif that == "?" then
      m, n = 0, 1
    elseif type(that) == "number" then
      if that < 0 then
        m, n = 0, -that
      else
        m = that
      end
    else
      m, n = that[1], that[2]
      if n == nil then
        n = m
      end
    end

    if n == nil then
      if m == 0 then
        return construct("*", self)
      elseif m == 1 then
        return construct("+", self)
      else
        local result = self
        for i = 3, m do
          result = result + self
        end
        return result + construct("+", self)
      end
    else
      if m == 0 then
        local result = construct("?", self)
        for i = 2, n do
          result = result + construct("?", self)
        end
        return result
      else
        local result = self
        for i = 2, m do
          result = result + self
        end
        for i = m + 1, n do
          result = result + construct("?", self)
        end
        return result
      end
    end
  end
end

function metatable:__sub(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  elseif self[0] == "[" and that[0] == "[" then
    local sub = that[1]
    local set = {}
    for byte in pairs(self[1]) do
      if not sub[byte] then
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    return construct("-", self, that)
  end
end

function metatable:__div(that)
  local self = pattern(self)
  if self[0] == "[" then
    return construct("/", self, that)
  else
    error "not supported"
  end
end

function metatable:__mod(that)
  local self = pattern(self)
  if self[0] == "%" then
    error "not supported"
  else
    return rawset(construct("%", self, that), "literal", rawget(self, "literal"))
  end
end

function metatable:__unm()
  local self = pattern(self)
  if self[0] == "[" then
    local neg = self[1]
    local set = {}
    for byte = 0x00, 0xFF do
      if not neg[byte] then
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    error "not supported"
  end
end

function metatable:__index(that)
  if self == module then
    return range(that)
  else
    error "not supported"
  end
end;

function metatable:__newindex(that)
  error "not supported"
end;

function metatable:__call(that)
  if self == module then
    if type(that) == "table" and getmetatable(that) ~= metatable then
      local result = set(that[1])
      for i = 2, #that do
        result = union(result, set(that[i]))
      end
      return result
    else
      return pattern(that)
    end
  else
    error "not supported"
  end
end

return module
