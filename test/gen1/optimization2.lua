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

local one = 1
local foo = "FOO"

local t = { one = 1, [one] = 2, foo = 3, [foo] = 4 }
print(#t, t.one, t[1], t.foo, t.FOO)
t.one = t.one * 1
t[one] = t[one] * 2
t.foo = t.foo * 3
t[foo] = t[foo] * 4
print(#t, t.one, t[1], t.foo, t.FOO)
