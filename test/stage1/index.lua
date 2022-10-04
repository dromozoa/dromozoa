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

local t = { foo = 1 }
local u = { foo = 2, bar = 3 }

setmetatable(t, {
  __index = function (self, k)
    return u[k]
  end;

  __newindex = function (self, k, v)
    u[k] = v
  end;
})

print(t.foo, u.foo, t.bar, u.bar, t.baz, u.baz)

t.foo = 4
t.bar = 5
t.baz = 6

print(t.foo, u.foo, t.bar, u.bar, t.baz, u.baz)
