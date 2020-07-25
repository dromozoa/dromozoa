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

local automaton = require "dromozoa.lexer.automaton"
local encode_char_class = require "dromozoa.lexer.encode_char_class"

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

local prec_table = {
  ["["] = 1;
  ["*"] = 2; ["?"] = 2;
  ["."] = 3;
  ["|"] = 4;
}
local prec_start = 5

local function to_pattern(self, parent_prec)
  local code = self[0]
  if code == "[" then
    return encode_char_class(self[1])
  else
    local prec = prec_table[code]
    local group = prec > parent_prec

    local buffer = {}
    local n = 0

    if group then
      n = n + 1; buffer[n] = "("
    end

    n = n + 1; buffer[n] = to_pattern(self[1], prec)

    if code ~= "." then
      n = n + 1; buffer[n] = code
    end

    local b = self[2]
    if b then
      n = n + 1; buffer[n] = to_pattern(b, prec)
    end

    if group then
      n = n + 1; buffer[n] = ")"
    end

    return table.concat(buffer)
  end
end

function class:to_pattern()
  return to_pattern(self, prec_start)
end

local function to_nfa(self, that, accept)
  local code = self[0]
  if code == "[" then
    local u = that:new_state()
    local v = that:new_state()
    that:new_transition(u, v, self[1])
    return u, v
  elseif code == "*" then
    local au, av = to_nfa(self[1], that, accept)
    local u = that:new_state()
    local v = that:new_state()
    that:new_transition(u, au)
    that:new_transition(u, v)
    that:new_transition(av, v)
    that:new_transition(av, au)
    return u, v
  elseif code == "?" then
    local au, av = to_nfa(self[1], that, accept)
    local u = that:new_state()
    local v = that:new_state()
    that:new_transition(u, au)
    that:new_transition(u, v)
    that:new_transition(av, v)
    return u, v
  elseif code == "." then
    local au, av = to_nfa(self[1], that, accept)
    local bu, bv = to_nfa(self[2], that, accept)
    that:new_transition(av, bu)
    return au, bv
  elseif code == "|" then
    local au, av = to_nfa(self[1], that, accept)
    local bu, bv = to_nfa(self[2], that, accept)
    local u = that:new_state()
    local v = that:new_state()
    that:new_transition(u, au)
    that:new_transition(u, bu)
    that:new_transition(av, v)
    that:new_transition(bv, v)
    return u, v
  end
end

function class:to_nfa(accept)
  if not accept then
    accept = 1
  end

  local that = automaton.nfa()
  local u, v = to_nfa(self, that, accept)
  that.start_state = u
  that.accept_states[v] = accept

  return that
end

return class
