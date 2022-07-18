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

local module = require "dromozoa.parser.grammar"

local dumper = require "dromozoa.commons.dumper"

local left = module.left
local _ = module.body

local x = {
  left "+";
  left "*";

  main
    = _"E"
    | _"F"
    ;

  E = _"E" "+" "E"
    | _"E" "*" "E"
    | _"-" "E" :prec "UNM" %[[action]]
    | _"id";
  F = _();
}

print(dumper.encode(x, { pretty = true, stable = true }))

--[====[
local module = {}
local timestamp = 0

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
]====]
