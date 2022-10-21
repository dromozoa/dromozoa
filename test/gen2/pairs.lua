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

local t = { foo = 1, bar = 2, "baz", "qux" }

local a = {}
local b = {}

for k, v in pairs(t) do
  if type(k) == "string" then
    a[v] = k
  else
    b[k] = v
  end
end

for i = 1, #a do
  print(a[i])
end
for i = 1, #b do
  print(a[i])
end

local t = setmetatable({}, { __pairs = function (t) return 1, 2 end })
local u = setmetatable({}, { __pairs = function (t) return 1, 2, 3, 4 end })
print(pairs(t))
print(pairs(u))
