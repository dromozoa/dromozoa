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

local function get_timestamp()
  timestamp = timestamp + 1
  return timestamp
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.precedence" }

local function Precedence(associativity, ...)
  return setmetatable({ timestamp = get_timestamp(), associativity = associativity, ... }, metatable)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

module.left = function (...)
  return Precedence("left", ...)
end

module.right = function (...)
  return Precedence("right", ...)
end

module.nonassoc = function (...)
  return Precedence("nonassoc", ...)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.bodies" }

local function Bodies(...)
  return setmetatable({ timestamp = get_timestamp(), ... }, metatable)
end

function metatable:__bor(that)
  self[#self + 1] = that
  return self
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.body" }

local function Body(...)
  return setmetatable({ timestamp = get_timestamp(), ... }, metatable)
end

function class:prec(that)
  self.prec = that
  return self
end

function metatable:__mod(that)
  self.action = that
  return self
end

function metatable:__bor(that)
  return Bodies(self, that)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

module.body = Body

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.list" }

local function List(...)
  return setmetatable({...}, metatable)
end

function class:add(...)
  local n = #self
  for i, v in ipairs {...} do
    self[n + i] = v
  end
  return self
end

function class:slice(i, j)
  return list(table.unpack(self, i, j))
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

---------------------------------------------------------------------------

local function grammar(token_names, that)
  local symbol_table = List "$"
  for _, name in ipairs(token_names) do
    symbol_table:add(name)
    symbol_table[name] = #symbol_table
  end
  local max_terminal_symbol = #symbol_table

  local data = List()
  for k, v in pairs(that) do
    data:add { k = k, v = v }
  end
  table.sort(data, function (a, b) return a.v.timestamp < b.v.timestamp end)

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v
    print(k)
  end

  -- return setmetatable({ symbol_names, symbol_table, that }, metatable)
  return {
    symbol_table = symbol_table;
    max_terminal_symbol = max_terminal_symbol;
  };
end

---------------------------------------------------------------------------

return setmetatable(module, { __call = function (_, ...) return grammar(...) end })
