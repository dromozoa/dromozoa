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

if a == 2 then
  globalThis.console:log "then"
else
  globalThis.console:log "else"
end

for i = 1, 3 do
  globalThis.console:log(i)
end

local x = { 11, 12, nil, nil, 13, 14 }
-- local x = {}
-- x[1] = 11
-- x[2] = 12
-- x[3] = "foo"
-- x[4] = true
-- x[5] = 13
-- x[6] = 14
for i = 1, 6 do
  globalThis.console:log(i, x[i])
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
  globalThis.console:log(i, v)
end

if a and b then
  globalThis.console:log "and"
end

-- globalThis.console:log("?", f(f(1,...,2,3)))
-- globalThis.console:log("?", f(f(1,2,3,...)))

-- t.f(f(1,2,3),4)
-- globalThis.console.log(t)

-- local no_such_fn="foo"
-- no_such_fn()

t = 12 t[1] = 42

local function g(...)
  globalThis.console:log(..., ...)
end

g()
g(1)
g(1,2)
g(1,2,3)

