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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local dumper = require "dromozoa.commons.dumper"

local module = {}
local timestamp = 0

---------------------------------------------------------------------------

local metatable = { __index = class, __name = "dromozoa.parser.bodies" }

module.bodies = function (a, b)
  timestamp = timestamp + 1
  return setmetatable({ [0] = "bodies", timestamp = timestamp, a, b }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.body" }

function class:prec(name)
  self.prec = name
  return self
end

function metatable:__div(action)
  self.action = action
  return self
end

function metatable:__bor(that)
  return module.bodies(self, that)
end

function metatable:__call(name)
  self[#self + 1] = name
  return self
end

module.body = function (name)
  timestamp = timestamp + 1
  return setmetatable({ [0] = "body", timestamp = timestamp, name }, metatable)
end

---------------------------------------------------------------------------

local metatable = { __index = class, __name = "dromozoa.parser.prec" }

local function new(prec, name)
  timestamp = timestamp + 1
  return setmetatable({ [0] = "prec", timestamp = timestamp, prec = prec, name }, metatable)
end

function metatable:__call(name)
  self[#self + 1] = name
  return self
end

module.left = function (name)
  return new("left", name)
end

module.right = function (name)
  return new("right", name)
end

module.nonassoc = function (name)
  return new("nonassoc", name)
end

---------------------------------------------------------------------------

function module:import(...)
  local result = { self }
  for i, k in ipairs {...} do
    result[i + 1] = self[k]
  end
  return table.unpack(result)
end

---------------------------------------------------------------------------

local grammar, _, right, left, nonassoc = module:import("body", "right", "left", "nonassoc")

local g = {
  left "*";
  left "+";
  right "UNM";

  ["E'"] = _"E";

  E = _"E" "*" "E"
    | _"E" "+" "E" / [[action]]
    | _"-" "E" :prec "UNM"
    ;
}

local h = {}
for k, v in pairs(g) do
  h[#h + 1] = { k, v }
end
table.sort(h, function (a, b) return a[2].timestamp < b[2].timestamp end)

print(dumper.encode(h, { pretty = true, stable = true }))
