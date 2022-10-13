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

local t = { 111, 42, 17, 23, foo = true, bar = "baz" }
table.sort(t)
for i, v in ipairs(t) do
  print(i, v)
end
print(t.foo)
print(t.bar)

local t = { {1}, {3}, {2}, {4} }
table.sort(t, function (a, b) return a[1] > b[1] end)
for i, v in ipairs(t) do
  print(i, v[1])
end
