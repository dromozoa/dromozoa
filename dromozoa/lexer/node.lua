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

local set = {}
for byte = 0x00, 0xFF do
  set[byte] = true
end
local any = new("[", set)

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

function metatable:__sub(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "[" and that[0] == "[" then
    local set = {}
    for byte in pairs(self[1]) do
      set[byte] = true
    end
    for byte in pairs(that[1]) do
      set[byte] = nil
    end
    return new("[", set)
  else
    return new("-", self, that)
  end
end

function metatable:__mul(that)
  local self = pattern(self)
  local that = pattern(that)
  return new(".", self, that)
end

function metatable:__pow(that)
  local t = type(that)
  if t == "number" then
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
  elseif t == "string" then
    if that == "*" then
      return new("*", self)
    elseif that == "+" then
      return new(".", self, new("*", self))
    elseif that == "?" then
      return new("?", self)
    end
  else
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

local encode_byte = {}
for byte = 0x00, 0xFF do
  encode_byte[byte] = ("\\x%02X"):format(byte)
end
local a, b = ("09"):byte(1, 2)
for byte = a, b do
  encode_byte[byte] = string.char(byte)
end
local a, b = ("AZ"):byte(1, 2)
for byte = a, b do
  encode_byte[byte] = string.char(byte)
end
local a, b = ("az"):byte(1, 2)
for byte = a, b do
  encode_byte[byte] = string.char(byte)
end

local function encode(self)
  local code = self[0]
  if code == "[" then
    local set = self[1]

    local n = 0
    for byte in pairs(set) do
      n = n + 1
    end

    if n == 256 then
      return ".", 1
    end

    if n == 1 then
      for byte in pairs(set) do
        return encode_byte[byte], 1
      end
    end

    local a = {}
    local b = {}
    local i = 0

    local neg = n > 127
    if neg then
      for byte = 0x00, 0xFF do
        if not set[byte] then
          if b[i] == byte - 1 then
            b[i] = byte
          else
            i = i + 1
            a[i] = byte
            b[i] = byte
          end
        end
      end
    else
      for byte = 0x00, 0xFF do
        if set[byte] then
          if b[i] == byte - 1 then
            b[i] = byte
          else
            i = i + 1
            a[i] = byte
            b[i] = byte
          end
        end
      end
    end

    local buffer = {}
    for i = 1, #a do
      local a = a[i]
      local b = b[i]
      if a == b then
        buffer[i] = encode_byte[a]
      elseif a == b - 1 then
        buffer[i] = encode_byte[a] .. encode_byte[b]
      else
        buffer[i] = encode_byte[a] .. "-" .. encode_byte[b]
      end
    end

    if neg then
      return "[^" .. table.concat(buffer) .. "]", 1
    else
      return "[" .. table.concat(buffer) .. "]", 1
    end
  elseif code == "?" or code == "*" then
    local pattern, prec = encode(self[1])
    if prec >= 2 then
      pattern = "(" .. pattern .. ")"
    end
    return pattern .. code, 2
  elseif code == "." then
    local a_pattern, a_prec = encode(self[1])
    local b_pattern, b_prec = encode(self[2])
    if a_prec > 3 then
      a_pattern = "(" .. a_pattern .. ")"
    end
    if b_prec > 3 then
      b_pattern = "(" .. b_pattern .. ")"
    end
    return a_pattern .. b_pattern, 3
  elseif code == "|" then
    local a_pattern, a_prec = encode(self[1])
    local b_pattern, b_prec = encode(self[2])
    return a_pattern .. "|" .. b_pattern, 4
  else
    error "..."
  end
end

function class:encode()
  return (encode(self))
end

return class
