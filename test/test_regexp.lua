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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local dumper = require "dromozoa.commons.dumper"

local module = {}

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern" }

local timestamp = 0

local function construct(code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

function metatable:__add(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  else
    return construct(".", self, that)
  end
end

function metatable:__sub(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
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
  local self = module.pattern(self)
  if self[0] == "[" then
    return construct("/", self, that)
  else
    error "not supported"
  end
end

function metatable:__mod(that)
  local self = module.pattern(self)
  if self[0] == "%" then
    error "not supported"
  else
    local result = construct("%", self, that)
    result.name = self.name
    return result
  end
end

function metatable:__bor(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
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

function metatable:__bnot(that)
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

function metatable:__call(that)
  if self[0] == "%" then
    error "not supported"
  else
    local m = that[1]
    local n = that[2]
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

function module.pattern(that)
  local t = type(that)
  if t == "string" then
    local self = construct("[", { [that:byte(1)] = true })
    for i = 2, #that do
      self = self + construct("[", { [that:byte(i)] = true })
    end
    self.name = that
    return self
  else
    return that
  end
end

function module.pattern_range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for byte = a, b do
      set[byte] = true
    end
  end
  return construct("[", set)
end

function module.pattern_set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return construct("[", set)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern.constructor" }

function metatable:__index(that)
  return module.pattern_range(that)
end

function metatable:__call(that)
  if type(that) == "table" then
    return module.pattern_set(that[1])
  else
    return module.pattern(that)
  end
end

module.constructor = setmetatable({}, metatable)

---------------------------------------------------------------------------

local _ = module.constructor

-- local x = _{"abc"} | _["09"]
-- local x = _"abc"{0,2}
-- local x = (_{"abc"} / "A" | "def") % "action"
-- local x = (_"a" | "b" | "c" | "z")/"transition" %"accept"
-- local x = (_["af"] + _"cz") %"action"
-- local x = _.ACac
-- local x = _"a"{0,0}
local x = _["ac"] | _["bd"]
local x = ~_["\0ad\255"]

print(dumper.encode(x, { pretty = true, stable = true }))
