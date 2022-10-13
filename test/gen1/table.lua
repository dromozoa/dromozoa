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

print(table.unpack {})
print(table.unpack {1})
print(table.unpack {1,2})
print(table.unpack {1,2,3})

print(table.concat {})
print(table.concat {1})
print(table.concat {1,2})
print(table.concat {1,2,3})

print(table.concat({}, ","))
print(table.concat({1}, ","))
print(table.concat({1,2}, ","))
print(table.concat({1,2,3}, ","))

print(table.concat({"foo", "bar", "baz", "qux"}, ",", 1, 4))
print(table.concat({"foo", "bar", "baz", "qux"}, ",", 1, 3))
print(table.concat({"foo", "bar", "baz", "qux"}, ",", 2, 4))
print(table.concat({"foo", "bar", "baz", "qux"}, ",", 2, 3))

--[[
local t = { [1] = "foo", [2] = "bar", [3] = "baz", 1, 2 }
print(#t, t[1], t[2], t[3])

local t = { [1] = "foo", [3] = "baz", nil, 2, nil }
print(#t, t[1], t[2], t[3])

local t = { [3] = "baz", 1, 2 }
print(#t, t[1], t[2], t[3])

local t = { 1, nil, 2, nil, nil, 3, nil, nil, nil }
print(#t)
]]
