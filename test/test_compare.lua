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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local tree_map = require "dromozoa.tree_map"

local typemap = {
  ["nil"]      = 0; -- LUA_TNIL
  ["boolean"]  = 1; -- LUA_TBOOLEAN
                    -- LUA_TLIGHTUSERDATA
  ["number"]   = 3; -- LUA_TNUMBER
  ["string"]   = 4; -- LUA_TSTRING
  ["table"]    = 5; -- LUA_TTABLE
  ["function"] = 6; -- LUA_TFUNCTION
  ["userdata"] = 7; -- LUA_TUSERDATA
  ["thread"]   = 8; -- LUA_TTHREAD
}

local function stable_pairs(t, compare, n)
  local metatable = getmetatable(t)
  if metatable and metatable.__name == "dromozoa.tree_map" then
    return metatable.__pairs(t)
  end

  local K = {}
  for k in pairs(t) do
    K[#K + 1] = k
  end
  table.sort(K, function (a, b)
    local c = compare(a, b, n)
    if c == 0 then
      error "table index is not unique"
    end
    return c < 0
  end)

  local i = 0
  return function (t)
    i = i + 1
    local k = K[i]
    return k, t[k]
  end, t
end

local function compare(a, b, n)
  local typename = type(a)
  local t = typemap[typename]
  local u = typemap[type(b)]
  if t ~= u then
    return t < u and -1 or 1
  end

  if a == b then
    return 0
  end

  if t == 1 then
    return b and -1 or 1
  elseif t == 3 then
    local a_is_nan = a ~= a
    local b_is_nan = b ~= b
    if b_is_nan then
      return a_is_nan and 0 or -1
    elseif a_is_nan then
      return 1
    end
    return a < b and -1 or 1
  elseif t == 4 then
    return a < b and -1 or 1
  end

  local s, c = pcall(function ()
    if a < b then
      return -1
    elseif a > b then
      return 1
    else
      return 0
    end
  end)
  if s and c then
    return c
  end

  if t == 5 then
    n = n == nil and 1 or n + 1
    if n > 2000 then
      error "too much recursion; possible loop"
    end

    local f, t, k, v = stable_pairs(b, compare, n)
    for j, u in stable_pairs(a, compare, n) do
      k, v = f(t, k)

      local c = compare(j, k, n)
      if c ~= 0 then
        return c
      end

      local c = compare(u, v, n)
      if c ~= 0 then
        return c
      end
    end

    k, v = f(t, k)
    return compare(nil, k, n)
  end

  error("attempt to compare two " .. typename .. " values")
end

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
check_eq(1, 1)
check_lt(1, "1")
check_lt(1, {1})
check_eq("1", "1")
check_lt("1", {1})
check_eq({1}, {1})

check_eq(false, false)
check_lt(false, true)

local minf = -math.huge
local zero = 0.0
local pinf = math.huge
local nan = pinf / pinf

check_lt(minf, zero)
check_lt(zero, pinf)
check_lt(pinf, nan)
check_eq(nan,  nan)
check_lt(nan,  "")

check_eq("", "")
check_lt("", "\0")
check_eq("\0", "\0")

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
-- print(message)
assert(not status)

local f = function () end
local g = function () end
assert(compare(f, f) == 0)

local status, message = pcall(function ()
  compare(f, g)
end)
-- print(message)
assert(not status)

local u = {}
local v = {}
u[1] = v
v[1] = u
compare(u, v)

