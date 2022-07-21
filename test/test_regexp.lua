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

function metatable:__add(that)
  local self = module.construct(self)
  local that = module.construct(that)
  if self[1] == "%" or that[1] == "%" then
    error "not supported"
  else
    return module.pattern(".", self, that)
  end
end

function metatable:__sub(that)
  local self = module.construct(self)
  local that = module.construct(that)
  if self[1] == "%" or that[1] == "%" then
    error "not supported"
  elseif self[1] == "[" and that[1] == "[" then
    local set = {}
    for byte in pairs(self[2]) do
      set[byte] = true
    end
    for byte in pairs(that[2]) do
      set[byte] = nil
    end
    return module.pattern("[", set)
  else
    return module.pattern("-", self, that)
  end
end

function metatable:__div(that)
  local self = module.construct(self)
  if self[1] ~= "[" then
    error "not supported"
  else
    return module.pattern("/", self, that)
  end
end


function metatable:__mod(that)
  local self = module.construct(self)
  if self[1] == "%" then
    error "not supported"
  else
    local result = module.pattern("%", self, that)
    result.literal = self.literal
    return result
  end
end

function metatable:__bor(that)
  local self = module.construct(self)
  local that = module.construct(that)
  if self[1] == "%" or that[1] == "%" then
    error "not supported"
  elseif self[1] == "[" and that[1] == "[" then
    local set = {}
    for byte in pairs(self[2]) do
      set[byte] = true
    end
    for byte in pairs(that[2]) do
      set[byte] = true
    end
    return module.character_class("[", set)
  else
    return module.pattern("|", self, that)
  end
end

function metatable:__bnot(that)
  if self[1] == "%" then
    error "not supported"
  else
    local set = self[2]
    local neg = {}
    for byte = 0x00, 0xFF do
      if not set[byte] then
        neg[byte] = true
      end
    end
    return module.pattern("[", neg)
  end
end

function metatable:__call(that)
  if self[1] == "%" then
    error "not supported"
  else
    local m = that[1]
    local n = that[2]
    if n == nil then
      if m == 0 then
        return module.pattern("*", self)
      elseif m == 1 then
        return module.pattern("+", self)
      else
        local result = self
        for i = 3, m do
          result = result + self
        end
        return result + module.pattern("+", self)
      end
    else
      if m == 0 then
        local result = module.pattern("?", self)
        for i = 2, n do
          result = result + module.pattern("?", self)
        end
        return result
      else
        local result = self
        for i = 2, m do
          result = result + self
        end
        for i = m + 1, n do
          result = result + module.pattern("?", self)
        end
        return result
      end
    end
  end
end

function module.pattern(...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, ... }, metatable)
end

---------------------------------------------------------------------------

function module.construct(that)
  local t = type(that)
  if t == "string" then
    local self = module.pattern("[", { [that:byte(1)] = true })
    for i = 2, #that do
      self = self + module.pattern("[", { [that:byte(i)] = true })
    end
    self.literal = that
    return self
  else
    return that
  end
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern.constructor" }

function metatable:__index(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for byte = a, b do
      set[byte] = true
    end
  end
  return module.pattern("[", set)
end

function metatable:__call(that)
  if type(that) == "table" then
    local that = that[1]
    local set = {}
    for i = 1, #that do
      set[that:byte(i)] = true
    end
    return module.pattern("[", set)
  else
    return module.construct(that)
  end
end

module.constructor = setmetatable({}, metatable)

---------------------------------------------------------------------------
--[[
  _"c"          character class
  _"literal"    literal
  _{"abcdef"}   set
  _["09AZaz"]   range
  _["\x00\xFF"] any
]]
---------------------------------------------------------------------------

local _ = module.constructor

-- local x = _{"abc"} | _["09"]
-- local x = _"abc"{0,1}
-- local x = (_{"abc"} / "A" | "def") % "action"
-- local x = (_"a" | "b" | "c" | "z")/"transition" %"accept"
local x = (_["af"] + _"cz") %"action"

print(dumper.encode(x, { pretty = true, stable = true }))
