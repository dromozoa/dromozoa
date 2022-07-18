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

function module.get_timestamp()
  timestamp = timestamp + 1
  return timestamp
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class; __name = "dromozoa.parser.precedence" }

local function new(associativity, ...)
  return setmetatable({
    [0] = "precedence";
    timestamp = module.get_timestamp();
    associativity = associativity;
    ...
  }, metatable)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.left(...)
  return new("left", ...)
end

function module.right(...)
  return new("right", ...)
end

function module.nonassoc(...)
  return new("nonassoc", ...)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.bodies" }

function metatable:__bor(that)
  self[#self + 1] = that
  return self
end

function module.bodies(...)
  return setmetatable({
    [0] = "bodies";
    timestamp = module.get_timestamp();
    ...
  }, metatable)
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
  return module.bodies(self, that)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.body(...)
  return setmetatable({
    [0] = "body";
    timestamp = module.get_timestamp();
    ...
  }, metatable)
end

---------------------------------------------------------------------------

return module
