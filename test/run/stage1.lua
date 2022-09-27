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

local v = 69

local f = function (a, b, c)
  return c, b, a
end

local a, b = f(1, 2, 3)

globalThis.console:log(a, b, c, d)

local t = {
  f = function (self, x, y, z)
    globalThis.console:log(self, x, y, z)
  end;
}

-- t.f(f(1,2,3), 4)
-- globalThis.console.log(t)

local no_such_fn="foo"
no_such_fn()

t = 12 t[1] = 42

