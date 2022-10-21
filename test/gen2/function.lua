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

local function f1()
  print "f1"
end

local function f2(a, b)
  print("f2", a, b)
end

f1()
f2("foo", "bar")
f1()

local t = { "a", "b", "c", "d" }
local function f(t, i)
  i = i + 1
  local v = t[i]
  if v == nil then
    return
  else
    return i, v
  end
end

for i, v in f, t, 0 do
  print(v)
end

for i, v in f, t, 0 do
  if i == 3 then
    break
  end
  print(v)
end

for i = 1, 4 do
  print(t[i])
end

t.f = function (a)
  if a then
    print "t.f(a)"
  else
    print "t.f()"
  end
end
t.f()
t:f()
