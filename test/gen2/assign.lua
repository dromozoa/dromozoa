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

local x, y, z = 1, 2, 3

local function f()
  local a, b, c = 1, 2, 3

  print(a, b, c)
  a, b, c = b * 4, c * 3, a * 2
  print(a, b, c)

  print(x, y, z)
  x, y, z = z * 4, y * 3, x * 2
  print(x, y, z)

  local t = { x = 1, y = 2, z = 3 }
  print(t.x, t.y, t.z)
  t.x, t.y, t.z = t.z * 4, t.y * 3, t.x * 2
  print(t.x, t.y, t.z)
end

f()

local t = { y = 17 }
local x = 42
t.x = x * 69
print(t.x, t.y)
t.x, t.y = t.y, t.x
print(t.x, t.y)
