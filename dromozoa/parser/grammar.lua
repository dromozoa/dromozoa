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

local array = require "dromozoa.array"
local tree_map = require "dromozoa.tree_map"
local tree_set = require "dromozoa.tree_set"

---------------------------------------------------------------------------

local module = {}

---------------------------------------------------------------------------

local timestamp = 0

local function construct(metatable, code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.precedence" }

function metatable:__call(that)
  assert(getmetatable(self) == metatable)
  assert(type(that) == "string")
  self[#self + 1] = that
  return self
end

function module.left(that)
  assert(type(that) == "string")
  return construct(metatable, "left", that)
end

function module.right(that)
  assert(type(that) == "string")
  return construct(metatable, "right", that)
end

function module.nonassoc(that)
  assert(type(that) == "string")
  return construct(metatable, "nonassoc", that)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.expect" }

function module.expect(that)
  assert(type(that) == "number")
  return construct(metatable, "expect", that)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.bodies" }

local function bodies(...)
  return construct(metatable, "bodies", ...)
end

function metatable:__add(that)
  assert(getmetatable(self) == metatable)
  assert(getmetatable(that).__name == "dromozoa.parser.grammar.body")
  self[#self + 1] = that
  return self
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.body" }

module.body = setmetatable({}, metatable)

local function body(that)
  -- that[0]がnilだったら
  if that == module.body then
    return construct(metatable, "body")
  elseif type(that) == "string" then
    return construct(metatable, "body", that)
  else
    assert(getmetatable(that) == metatable)
    assert(that.timestamp ~= nil)
    return that
  end
end

function class:prec(that)
  local self = body(self)
  assert(type(that) == "string")
  assert(self.precedence == nil)
  self.precedence = that
  return self
end

function metatable:__add(that)
  local self = body(self)
  assert(getmetatable(that) == metatable)
  return bodies(self, that)
end

function metatable:__mod(that)
  local self = body(self)
  assert(type(that) == "string")
  assert(self.semantic_action == nil)
  self.semantic_action = that
  return self
end

function metatable:__call(that)
  local self = body(self)
  assert(type(that) == "string")
  self[#self + 1] = that
  return self
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar" }

function metatable:__call(token_names, that)
  -- TODO token_namesがarrayの場合は？
  -- TODO $custom_dataに対応する

  local symbol_names = array()
  local symbol_table = {}
  for _, name in ipairs(token_names) do
    if symbol_table[name] ~= nil then
      error("symbol " .. name .. " redefined as a terminal")
    end
    symbol_table[name] = symbol_names:append(name):size()
  end
  local max_terminal_symbol = symbol_names:append "$":size()

  local expect_sr
  local precedence = 0
  local precedence_table = tree_map()
  local symbol_precedences = {}
  for _, v in ipairs(that) do
    if v[0] == "expect" then
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.expect")
      expect_sr = v[1]
    else
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.precedence")
      precedence = precedence + 1
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol == nil then
          precedence_table:assign(name, { name = name, precedence = precedence, associativity = v[0] })
        else
          symbol_precedences[symbol] = { name = name, precedence = precedence, associativity = v[0] }
        end
      end
    end
  end

  local data = array()
  for k, v in pairs(that) do
    if type(k) == "string" then
      data:append { timestamp = v.timestamp, k = k, v = v }
    end
  end
  data:sort(function (a, b) return a.timestamp < b.timestamp end)

  local start_head = symbol_names:append(data:get(1).k .. "'"):size()
  local start_body = start_head + 1

  for _, u in data:ipairs() do
    local k = u.k
    local v = u.v
    if symbol_table[k] ~= nil then
      error("symbol " .. k .. " redefined as a nonterminal")
    end
    local symbol = symbol_names:append(k):size()
    symbol_table[k] = symbol
    u.k = symbol
    if v[0] == "body" then
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.body")
      u.v = bodies(v)
    else
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.bodies")
    end
  end

  local productions = tree_set(function (a, b)
    if a.head ~= b.head then
      return a.head < b.head and -1 or 1
    end
    assert(a.head_index ~= b.head_index)
    return a.head_index < b.head_index and -1 or 1
  end):insert { head = start_head, head_index = 1, body = array(start_body) }

  local used_symbols = {
    [max_terminal_symbol] = true;
    [start_head] = true;
    [start_body] = true;
  }
  local used_precedences = {}

  for _, u in data:ipairs() do
    for i, v in ipairs(u.v) do
      local body = array()
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol == nil then
          error("symbol " .. name .. " not defined")
        end
        body:append(symbol)
        used_symbols[symbol] = true
      end
      local production = { head = u.k, head_index = i, body = body, semantic_action = v.semantic_action }
      if v.precedence ~= nil then
        local precedence = precedence_table:find(v.precedence)
        if precedence == nil then
          error("precedence " .. v.precedence .. " not defined")
        end
        production.precedence = precedence
        used_precedences[v.precedence] = true
      end
      productions:insert(production)
    end
  end

  for i, v in symbol_names:ipairs() do
    if used_symbols[i] == nil then
      error("symbol " .. v .. " not used")
    end
  end
  for k in precedence_table:pairs() do
    if used_precedences[k] == nil then
      error("precedence " .. k .. " not used")
    end
  end

  return {
    symbol_names = symbol_names;
    max_terminal_symbol = max_terminal_symbol;
    expect_sr = expect_sr;
    symbol_precedences = symbol_precedences;
    productions = productions;
  }
end

return setmetatable(module, metatable)
