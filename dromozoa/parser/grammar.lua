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

local module = {}

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.list" }

function class:append(...)
  local n = #self
  for i, v in ipairs {...} do
    self[n + i] = v
  end
  return self
end

function module.list(...)
  return setmetatable({...}, metatable)
end

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local metatable = { __name = "dromozoa.parser.grammar.map" }

function metatable:__newindex(k, v)
  local priv = private[self]
  local priv_set = priv.set
  if not priv_set[k] then
    priv[#priv + 1] = k
    priv_set[k] = true
  end
  rawset(self, k, v)
end

function metatable:__pairs()
  local priv = private[self]
  local index = 0
  return function (self)
    local n = #priv
    while index < n do
      index = index + 1
      local k = priv[index]
      local v = self[k]
      if v ~= nil then
        return k, v
      end
    end
  end, self
end

function module.map()
  local self = setmetatable({}, metatable)
  private[self] = { set = {} }
  return self
end

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

function metatable:__bor(that)
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
  self.action = that
  return self
end

function metatable:__bor(that)
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

-- eof = 1
--   eofはsymbol_tableからのマップをもたない
--   "$"というトークンを作成してもだいじょうぶ
-- min_nonterminal_symbol = max_terminal_symbol + 1
-- argumented_start_symbol = min_nonterminal_symbol

local function grammar(token_names, that)
  local symbol_names = module.list()
  local symbol_table = module.map()
  for _, name in ipairs(token_names) do
    symbol_table[name] = #symbol_names:append(name)
  end
  local max_terminal_symbol = #symbol_names

  local data = module.list()
  for k, v in pairs(that) do
    data:append { k = k, v = v }
  end
  table.sort(data, function (a, b) return a.v.timestamp < b.v.timestamp end)

  local productions = module.list()

  local precedence = 0
  local precedence_table = module.map()
  local symbol_precedences = module.map()
  local production_precedences = module.map()
  local semantic_actions = module.map()

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v
    local metatable = getmetatable(v)
    local metaname = metatable and metatable.__name

    if metaname == "dromozoa.parser.grammar.precedence" then
      precedence = precedence + 1
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol and symbol < max_terminal_symbol then
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
        error("symbol " .. k .. " redeclared as a nonterminal")
      end
      local symbol = #symbol_names:append(k)
      symbol_table[k] = symbol

      if metaname == "dromozoa.parser.grammar.body" then
        v = module.bodies(v)
      end
      for _, body in ipairs(v) do
        productions:append { head = symbol, body = body }
      end
    end
  end

  for i, production in ipairs(productions) do
    local body = module.list()
    for _, name in ipairs(production.body) do
      local symbol = symbol_table[name];
      if not symbol then
        error("symbol " .. name .. " is used, but is not defined as a token and has no rules")
      end
      body:append(symbol)
    end
    local name = production.body.precedence
    if name then
      local precedence = precedence_table[name]
      if not precedence then
        error("token for :prec is not defined: " .. name)
      end
      production_precedences[i] = precedence
    end
    semantic_actions[i] = production.body.action
    production.body = body
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

---------------------------------------------------------------------------

return setmetatable(module, { __call = function (_, ...) return grammar(...) end })
