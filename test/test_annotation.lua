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
local parse = require "dromozoa.annotation.parse"

local dump

local function dump_tuple(buffer, u)
  if #u == 0 then
    append(buffer, "()")
  else
    append(buffer, "(")
    for i, v in ipairs(u) do
      if i > 1 then
        append(buffer, ",")
      end
      dump(buffer, v)
    end
    append(buffer, ")")
  end
end

function dump(buffer, u)
  if type(u) == "table" then
    dump_tuple(buffer, u[1])
    append(buffer, "->")
    dump_tuple(buffer, u[2])
  else
    append(buffer, u)
  end
end

local buffer = {}

local t = parse "(i32,f64)->()"
assert(#t[1] == 2)
assert(t[1][1] == "i32")
assert(t[1][2] == "f64")
assert(#t[2] == 0)
dump(buffer, t)
append(buffer, "\n")

local s = [[
( ( i64 ) -> ( i32 ) , i64 ) -> ( boolean, i32 )
]]
local t = parse(s)
dump(buffer, t)
append(buffer, "\n")

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
(i32,f64)->()
((i64)->(i32),i64)->(boolean,i32)
]])
