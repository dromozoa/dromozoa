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

local tree_map = require "dromozoa.tree_map"
local tree_set = require "dromozoa.tree_set"
local production_set = require "dromozoa.parser.production_set"

---------------------------------------------------------------------------

-- TODO 共通コード
local function append(t, ...)
  local m = #t
  local n = select("#", ...)

  for i = 1, n do
    local v = select(i, ...)
    assert(v ~= nil)
    t[m + i] = v
  end

  return m + n
end

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
  if type(that) == "string" then
    return construct(metatable, "body", that)
  else
    assert(getmetatable(that) == metatable)
    if that[0] == nil then
      assert(that.timestamp == nil)
      return construct(metatable, "body")
    else
      assert(that.timestamp ~= nil)
      return that
    end
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
  local symbol_names = {}
  local symbol_table = {}
  for _, name in token_names:ipairs() do
    if symbol_table[name] ~= nil then
      error("symbol " .. name .. " redefined as a terminal")
    end
    symbol_table[name] = append(symbol_names, name)
  end
  local max_terminal_symbol = append(symbol_names, "$")

  local custom_data = {}
  local expect_sr
  local precedence = 0
  local precedence_table = tree_map()
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
        if symbol == nil then
          precedence_table:assign(name, { name = name, precedence = precedence, associativity = v[0] })
        else
          symbol_precedences[symbol] = { name = name, precedence = precedence, associativity = v[0] }
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
    if symbol_table[k] ~= nil then
      error("symbol " .. k .. " redefined as a nonterminal")
    end
    local symbol = append(symbol_names, k)
    symbol_table[k] = symbol
    u.k = symbol
    if v[0] == "body" then
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.body")
      u.v = bodies(v)
    else
      assert(getmetatable(v).__name == "dromozoa.parser.grammar.bodies")
    end
  end

  -- 生成規則は番号 (index) を持つ
  -- 生成規則は頭部と本体で一意である
  -- 頭部は非終端記号、本体は記号列で表される。
  -- 同じ生成規則が存在するのはエラー時だけなので、挿入時に存在しないことを確認すればいい。

  -- local productions = tree_set(function (a, b)
  --   if a.head ~= b.head then
  --     return a.head < b.head and -1 or 1
  --   end
  --   assert(a.head_index ~= b.head_index)
  --   return a.head_index < b.head_index and -1 or 1
  -- end):insert { head = start_head, head_index = 1, body = { start_body } }

  local productions = production_set()
  productions:insert { head = start_head, body = { start_body } }

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
        if symbol == nil then
          error("symbol " .. name .. " not defined")
        end
        append(body, symbol)
        used_symbols[symbol] = true
      end
      local production = { head = u.k, body = body, semantic_action = v.semantic_action }
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

  for i, v in ipairs(symbol_names) do
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
    symbol_table = symbol_table;
    max_terminal_symbol = max_terminal_symbol;
    custom_data = custom_data;
    expect_sr = expect_sr;
    symbol_precedences = symbol_precedences;
    productions = productions;
  }
end

return setmetatable(module, metatable)
