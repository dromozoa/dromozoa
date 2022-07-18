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
  self.prec = that
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

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.list" }

function class:add(...)
  local n = #self
  for i, v in ipairs {...} do
    self[n + i] = v
  end
  return self
end

function class:slice(i, j)
  return module.list(table.unpack(self, i, j))
end

function class:each(fn)
  return function (self, index)
    for i = index + 1, #self do
      local v = fn(self[i])
      if v then
        return i, v
      end
    end
  end, self, 0
end

function module.list(...)
  return setmetatable({...}, metatable)
end

---------------------------------------------------------------------------

local function grammar(token_names, that)
  local symbol_table = module.list "$"
  for _, name in ipairs(token_names) do
    symbol_table:add(name)
    symbol_table[name] = #symbol_table
  end
  local max_terminal_symbol = #symbol_table

  local data = module.list()
  for k, v in pairs(that) do
    data:add { k = k, v = v }
  end
  table.sort(data, function (a, b) return a.v.timestamp < b.v.timestamp end)

  local precedences = module.list()
  local productions = module.list()

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v

    local metatable = getmetatable(v)
    local metaname = metatable and metatable.__name
    assert(type(metaname) == "string")
    assert(metaname:find "^dromozoa%.parser%.grammar%.")

    if metaname == "dromozoa.parser.grammar.precedence" then
      precedences:add(v)
    else
      if metaname == "dromozoa.parser.grammar.body" then
        v = module.bodies(v)
        metaname = "dromozoa.parser.grammar.bodies"
      end
      assert(metaname == "dromozoa.parser.grammar.bodies")

      assert(not symbol_table[k])
      symbol_table:add(k)
      symbol_table[k] = #symbol_table

      for _, body in ipairs(v) do
        productions:add { head = k, body = body }
      end
    end
  end

  for _, production in ipairs(productions) do
    for _, name in ipairs(production.body) do
      assert(symbol_table[name], "not defined " .. name)
    end
  end

  return {
    symbol_table = symbol_table;
    max_terminal_symbol = max_terminal_symbol;
    precedences = precedences;
    productions = productions;
  };
end

---------------------------------------------------------------------------

return setmetatable(module, { __call = function (_, ...) return grammar(...) end })
