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

local tree = require "dromozoa.tree"
-- local tree_map = require "dromozoa.tree_map"

---------------------------------------------------------------------------
--[====[
---------------------------------------------------------------------------

local nan = math.huge / math.huge

local status, message = pcall(function ()
  tree_map()[nil] = nil
end)
-- print(message)
assert(not status)

local status, message = pcall(function ()
  tree_map()[nan] = nan
end)
-- print(message)
assert(not status)

local t = tree_map()
t.foo = 1
t.bar = 2
t.baz = 3
t.qux = 4
assert(t().size == 4)

t.bar = nil
assert(t().size == 3)

assert(t.foo == 1)
assert(t.bar == nil)
assert(t.baz == 3)
assert(t.qux == 4)
assert(t[nil] == nil)
assert(t[nan] == nil)
assert(t.compare == nil)
assert(t.size == nil)

local buffer = {}
for k, v in t().next, t() do
  buffer[#buffer + 1] = k .. "=" .. v
end
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")

local buffer = {}
for k, v in t():each("c") do
  buffer[#buffer + 1] = k .. "=" .. v
end
assert(table.concat(buffer, ";") == "foo=1;qux=4")

local buffer = {}
for k, v in t():each(nil, "q") do
  buffer[#buffer + 1] = k .. "=" .. v
end
assert(table.concat(buffer, ";") == "baz=3;foo=1")

local buffer = {}
for k, v in t().next, t() do
  buffer[#buffer + 1] = k .. "=" .. v
  t[k] = nil
end
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")

---------------------------------------------------------------------------

local t = tree_map()
t"foo""bar".baz = 42
assert(t.foo.bar.baz == 42)

t(1)(2)(3)[4] = "qux"
assert(t[1][2][3][4] == "qux")

---------------------------------------------------------------------------

local n = 0

local function f(...)
  assert(select("#", ...) == 0)
  n = n + 1
  return n * 2
end

assert(n == 0)
assert(t("xxx", f) == 2)
assert(n == 1)
assert(t("xxx", f) == 2)
assert(n == 1)

]====]
