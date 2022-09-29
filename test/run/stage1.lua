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

local D = dromozoa

local function log(a, b, c, d)
  print(a,b,c,d)
end

local v = 69

local f = function (a, b, c)
  return c, b, a
end

local a, b = f(1, 2, 3)
print(a, b, c, d)

local t = {
  f = function (self, x, y, z)
    print(self, x, y, z)
  end;
}

if a == 2 then
  print "then"
else
  print "else"
end

for i = 1, 3 do
  print(i)
end

local x = { 11, 12, 13, 14, 15, 16 }
print(#x)
local x = { 11, 12, nil, nil, 13, 14 }
-- local x = {}
-- x[1] = 11
-- x[2] = 12
-- x[3] = "foo"
-- x[4] = true
-- x[5] = 13
-- x[6] = 14
for i = 1, 6 do
  print(i, x[i])
end

local function impl_ipairs(t, i)
  i = i + 1
  local v = t[i]
  if v == nil then
    return
  else
    return i, v
  end
end

for i, v in impl_ipairs, x, 0 do
  print(i, v)
end

if a and b then
  print "and"
end

-- local no_such_fn="foo"
-- no_such_fn()

-- t = 12 t[1] = 42

local function g(...)
  print(..., ...)
end

g()
g(1)
g(1,2)
g(1,2,3)

if 0x1p+3 == 8 then
  print "ok"
end
