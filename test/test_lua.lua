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

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local i = 0;
local function f()
  i = i + 1
  return i
end

--[[
  newtable
  call f       ; {1}
  call f       ; {1,2}
  call f       ; {1,2,3}
  setfield "a" ; {1,2}
  call f       ; {1,2,4}
  call f       ; {1,2,4,5}
  setfield "b" ; {1,2,4}
  call f       ; {1,2,4,6}
  call f       ; {1,2,4,6,7}
  setfield "c" ; {1,2,4,6}
  call f       ; {1,2,4,6,8}
  call f       ; {1,2,4,6,8,9}
  setlist
]]

local x = {
  f();
  f();
  a = f();
  f();
  b = f();
  f();
  c = f();
  f();
  f();
}

local y = {}
for k, v in pairs(x) do
  y[#y + 1] = { k, v }
end
table.sort(y, function (a, b) return a[2] < b[2] end)

local z = { 1, 2, "a", 3, "b", 4, "c", 5, 6 }

assert(#y == #z)
for i = 1, #y do
  if debug then
    print(y[i][1], y[i][2])
  end
  assert(y[i][1] == z[i])
  assert(y[i][2] == i)
end
