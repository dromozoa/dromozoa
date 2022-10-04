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

local a = function ()
  print("a")
end

local b = {}
b.f = function (x)
  assert(x == nil or x == b)
  print("b.f", x and "x")
end

local c = {}
setmetatable(c, {
  __call = function (x)
    assert(x == nil or x == c)
    print("c.__call", x and "x")
  end;
})

local d = {}
d.f = setmetatable({}, {
  __call = function (x, y)
    assert(x == d.f)
    assert(y == nil or y == d)
    print("d.f.__call", x and "x", y and "y")
  end;
})

a()
b.f()
b:f()
c()
d.f()
d:f()
