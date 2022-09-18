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

local append = require "dromozoa.append"
local production_set = require "dromozoa.parser.production_set"

---------------------------------------------------------------------------

local module = {}

---------------------------------------------------------------------------

local timestamp = 0

local function construct(metatable, code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.expect" }

function module.expect(that)
  assert(type(that) == "number")
  return construct(metatable, "expect", that)
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

local metatable = { __name = "dromozoa.parser.grammar.bodies" }

function metatable:__add(that)
  assert(getmetatable(self) == metatable)
  assert(getmetatable(that).__name == "dromozoa.parser.grammar.body")
  self[#self + 1] = that
  return self
end

function module.bodies(...)
  return construct(metatable, "bodies", ...)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.body" }

function class:prec(that)
  local self = module.body(self)
  assert(type(that) == "string")
  assert(not self.precedence)
  self.precedence = that
  return self
end

function metatable:__add(that)
  local self = module.body(self)
  assert(getmetatable(that) == metatable)
  return module.bodies(self, that)
end

function metatable:__mod(that)
  local self = module.body(self)
  assert(type(that) == "string")
  assert(not self.semantic_action)
  self.semantic_action = that
  return self
end

function metatable:__call(that)
  local self = module.body(self)
  assert(type(that) == "string")
  self[#self + 1] = that
  return self
end

function module.body(that)
  if that == nil or type(that) == "string" then
    return construct(metatable, "body", that)
  else
    assert(getmetatable(that) == metatable)
    return that
  end
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar" }

function metatable:__call(token_names, that)
  local symbol_names = {}
  local symbol_table = {}
  for _, name in ipairs(token_names) do
    if symbol_table[name] then
      error("symbol " .. name .. " redefined as a terminal")
    end
    symbol_table[name] = append(symbol_names, name)
  end
  local max_terminal_symbol = append(symbol_names, "$")

  local custom_data = {}
  local expect_sr
  local precedence = 0
  local precedence_table = {}
  local symbol_precedences = {}
  for _, v in ipairs(that) do
    if type(v) == "string" then
      append(custom_data, v .. "\n")
    elseif v[0] == "expect" then
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.expect")
      expect_sr = v[1]
    else
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.precedence")
      precedence = precedence + 1
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol then
          symbol_precedences[symbol] = { name = name, precedence = precedence, associativity = v[0] }
        else
          precedence_table[name] = { name = name, precedence = precedence, associativity = v[0] }
        end
      end
    end
  end

  local data = {}
  for k, v in pairs(that) do
    if type(k) == "string" then
      append(data, { timestamp = v.timestamp, k = k, v = v })
    end
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local start_head = append(symbol_names, data[1].k .. "'")
  local start_body = start_head + 1

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v
    if symbol_table[k] then
      error("symbol " .. k .. " redefined as a nonterminal")
    end
    local symbol = append(symbol_names, k)
    symbol_table[k] = symbol
    u.k = symbol
    if v[0] == "body" then
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.body")
      u.v = module.bodies(v)
    else
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.bodies")
    end
  end

  local productions = production_set { head = start_head, body = { start_body } }
  local used_symbols = {
    [max_terminal_symbol] = true;
    [start_head] = true;
    [start_body] = true;
  }
  local used_precedences = {}

  for _, u in ipairs(data) do
    for i, v in ipairs(u.v) do
      local body = {}
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if not symbol then
          error("symbol " .. name .. " not defined")
        end
        append(body, symbol)
        used_symbols[symbol] = true
      end
      local production = { head = u.k, body = body, semantic_action = v.semantic_action }
      if v.precedence then
        local precedence = precedence_table[v.precedence]
        if not precedence then
          error("precedence " .. v.precedence .. " not defined")
        end
        production.precedence = precedence
        used_precedences[v.precedence] = true
      end
      productions:insert(production)
    end
  end

  for i, v in ipairs(symbol_names) do
    if not used_symbols[i] then
      error("symbol " .. v .. " not used")
    end
  end
  for k in pairs(precedence_table) do
    if not used_precedences[k] then
      error("precedence " .. k .. " not used")
    end
  end

  return {
    symbol_names = symbol_names;
    symbol_table = symbol_table;
    max_terminal_symbol = max_terminal_symbol;
    custom_data = custom_data;
    expect_sr = expect_sr;
    symbol_precedences = symbol_precedences;
    productions = productions;
  }
end

return setmetatable(module, metatable)
