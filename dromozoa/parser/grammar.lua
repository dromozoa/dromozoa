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

function get_timestamp()
  timestamp = timestamp + 1
  return timestamp
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.precedence" }

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

local function precedence(associativity, ...)
  return setmetatable({ timestamp = get_timestamp(), associativity = associativity, ... }, metatable)
end

function module.left(...)
  return precedence("left", ...)
end

function module.right(...)
  return precedence("right", ...)
end

function module.nonassoc(...)
  return precedence("nonassoc", ...)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.bodies" }

function metatable:__bor(that)
  self[#self + 1] = that
  return self
end

local function bodies(...)
  return setmetatable({ timestamp = get_timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.body" }

function class:prec(that)
  self.prec = that
  return self
end

function metatable:__mod(that)
  self.action = that
  return self
end

function metatable:__bor(that)
  return bodies(self, that)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.body(...)
  return setmetatable({ timestamp = get_timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

local function grammar(token_names, that)
  return setmetatable({ token_names, that }, metatable)
end

---------------------------------------------------------------------------

return setmetatable(module, { __call = function (_, ...) return grammar(...) end })
