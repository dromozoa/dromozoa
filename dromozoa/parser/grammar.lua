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

local list = require "dromozoa.list"
-- local tree_map = require "dromozoa.tree_map"

local module = {}

---------------------------------------------------------------------------

local timestamp = 0

function module.timestamp()
  timestamp = timestamp + 1
  return timestamp
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.precedence" }

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.precedence(associativity, ...)
  return setmetatable({ timestamp = module.timestamp(), associativity = associativity, ... }, metatable)
end

function module.left(...)
  return module.precedence("left", ...)
end

function module.right(...)
  return module.precedence("right", ...)
end

function module.nonassoc(...)
  return module.precedence("nonassoc", ...)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.bodies" }

function metatable:__add(that)
  self[#self + 1] = that
  return self
end

function module.bodies(...)
  return setmetatable({ timestamp = module.timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.body" }

function class:prec(that)
  self.precedence = that
  return self
end

function metatable:__mod(that)
  self.semantic_action = that
  return self
end

function metatable:__add(that)
  return module.bodies(self, that)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.body(...)
  return setmetatable({ timestamp = module.timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

function module.grammar(token_names, that)
  local symbol_names = list()
  local symbol_table = {}
  for _, name in ipairs(token_names) do
    if symbol_table[name] then
      error("symbol " .. name .. " redefined as a terminal")
    end
    symbol_table[name] = #symbol_names:append(name)
  end
  local max_terminal_symbol = #symbol_names:append "$"

  local data = list()
  for k, v in pairs(that) do
    data:append { k = k, v = v }
  end
  table.sort(data, function (a, b) return a.v.timestamp < b.v.timestamp end)

  local augumented_start_head = #symbol_names:append ""
  local augumented_start_body = augumented_start_head + 1

  local productions = list { head = augumented_start_head, body = list() }
  local precedence = 0
  local precedence_table = {}
  local symbol_precedences = {}

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v
    local metaname = getmetatable(v).__name

    if metaname == "dromozoa.parser.grammar.precedence" then
      precedence = precedence + 1
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol and symbol <= max_terminal_symbol then
          symbol_precedences[symbol] = {
            precedence = precedence;
            associativity = v.associativity;
          }
        else
          precedence_table[name] = {
            precedence = precedence;
            associativity = v.associativity;
          }
        end
      end
    else
      if symbol_table[k] then
        error("symbol " .. k .. " redefined as a nonterminal")
      end
      local symbol = #symbol_names:append(k)
      symbol_table[k] = symbol

      if metaname == "dromozoa.parser.grammar.body" then
        productions:append { head = symbol, body = v }
      else
        for _, body in ipairs(v) do
          productions:append { head = symbol, body = body }
        end
      end
    end
  end

  -- TODO このふたつはシークエンスを保証したほうがよい？
  local production_precedences = {}
  local semantic_actions = {} -- TODO semantic_actionがnilのときはどうする？
  local used_symbols = {}
  local used_precedences = {}

  for i, production in ipairs(productions) do
    local name = production.body.precedence
    if name then
      local precedence = precedence_table[name]
      if not precedence then
        error("precedence " .. name .. " not defined")
      end
      production_precedences[i] = precedence
      used_precedences[name] = true
    end
    semantic_actions[i] = production.body.semantic_action

    local body = list()
    for _, name in ipairs(production.body) do
      local symbol = symbol_table[name];
      if not symbol then
        error("symbol " .. name .. " not defined")
      end
      body:append(symbol)
      used_symbols[symbol] = true
    end
    production.body = body
  end

  productions[1].body:append(augumented_start_body)
  symbol_names[augumented_start_head] = symbol_names[augumented_start_body] .. "'"

  used_symbols[max_terminal_symbol] = true
  used_symbols[augumented_start_head] = true
  used_symbols[augumented_start_body] = true

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
    max_nonterminal_symbol = #symbol_names;
    productions = productions;
    symbol_precedences = symbol_precedences;
    production_precedences = production_precedences;
    semantic_actions = semantic_actions;
  };
end

return setmetatable(module, { __call = function (_, ...) return module.grammar(...) end })
