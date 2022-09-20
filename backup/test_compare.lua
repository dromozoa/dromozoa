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

local verbose = os.getenv "VERBOSE" == "1"

local compare = require "dromozoa.compare"

local function check_lt(a, b)
  assert(compare(a, b) < 0)
  assert(compare(b, a) > 0)
end

local function check_eq(a, b)
  assert(compare(a, b) == 0)
  assert(compare(b, a) == 0)
end

local function check_gt(a, b)
  assert(compare(a, b) > 0)
  assert(compare(b, a) < 0)
end

check_eq(nil, nil)
check_lt(nil, true)
check_lt(nil, 1)
check_lt(nil, "1")
check_lt(nil, {1})

check_eq(true, true)
check_lt(true, 1)
check_lt(true, "1")
check_lt(true, {1})

check_eq("1", "1")
check_lt("1", 1)
check_lt("1", {1})

check_eq(1, 1)
check_lt(1, {1})

check_eq({1}, {1})

check_eq(false, false)
check_lt(false, true)

local minf = -math.huge
local zero = 0.0
local pinf = math.huge
local nan = pinf / pinf

check_eq("", "")
check_lt("", "\0")
check_eq("\0", "\0")

check_lt(minf, zero)
check_lt(zero, pinf)
check_lt(pinf, nan)
check_eq(nan,  nan)
check_lt(nan,  {})

check_eq({}, {})
check_lt({}, {17})
check_eq({17}, {17})
check_lt({17}, {42})
check_lt({17,23}, {42})
check_eq({abc=1}, {abc=1})
check_lt({abc=1}, {abc=2})
check_lt({aaa=1}, {abc=1})

local status, message = pcall(function ()
  compare({[{}]=1,[{}]=2}, {[{}]=1,[{}]=2})
end)
if verbose then
  print(message)
end
assert(not status)

local f = function () end
local g = function () end
assert(compare(f, f) == 0)

local status, message = pcall(function ()
  compare(f, g)
end)
if verbose then
  print(message)
end
assert(not status)

local u = {}
local v = {}
u[1] = v
v[1] = u
local status, message = pcall(function ()
  compare(u, v)
end)
if verbose then
  print(message)
end
assert(not status)
