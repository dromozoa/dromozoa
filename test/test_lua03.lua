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

local p_inf = math.huge
local m_inf = -math.huge
local p_zero = 0/p_inf
local m_zero = 0/m_inf
local i_zero = 0
local nan = p_inf/p_inf

print(type(m_zero), type(i_zero), type(nan))

print(p_inf, m_inf, p_zero, m_zero, i_zero, nan)
print(p_inf == p_inf, m_inf == m_inf)
print(p_zero == m_zero, p_zero == i_zero, m_zero == i_zero)
print(p_zero < m_zero, p_zero < i_zero, m_zero < i_zero)
print(p_zero > m_zero, p_zero > i_zero, m_zero > i_zero)
print(nan == nan)

local t = {}
t[p_inf] = "p_inf"
t[m_inf] = "m_inf"

t[p_zero] = "p_zero"
print "--"
for k, v in pairs(t) do
  print(k, v)
end

t[m_zero] = "m_zero"
print "--"
for k, v in pairs(t) do
  print(k, v)
end

t[i_zero] = "i_zero"
print "--"
for k, v in pairs(t) do
  print(k, v)
end

assert(not pcall(function () t[nan] = "nan" end))

t["nil"] = nil
t["nan"] = nan
print "--"
for k, v in pairs(t) do
  print(k, v)
end

print "=="

local u = setmetatable({}, {
  __eq = function (a, b)
    print("u.__eq", a, b)
    return false
  end;
  __lt = function (a, b)
    print("u.__lt", a, b)
    return false
  end;
})

local v = setmetatable({}, {
  __eq = function (a, b)
    print("v.__eq", a, b)
    return false
  end;
  __lt = function (a, b)
    print("v.__lt", a, b)
    return false
  end;
})

local f = assert(io.open "/dev/null")

local x = u == nil
local x = u == true
local x = u == 42
local x = u == "foo"
local x = u == v
local x = u == function () end
local x = u == f
local x = u == coroutine.create(function () end)

-- Lua 5.1/LuaJITでは、左右の型が同じで、かつどちらも同じメタメソッドを持ってい
-- なければならない。
-- Lua 5.2以降では、メタメソッドが定義されていれば呼ばれる

-- local x = u < nil
-- local x = u < true
-- local x = u < 42
-- local x = u < "foo"
-- local x = u < v
-- local x = u < function () end
-- local x = u < f
-- local x = u < coroutine.create(function () end)

-- local x = u > nil
-- local x = u > true
-- local x = u > 42
-- local x = u > "foo"
-- local x = u > v
-- local x = u > function () end
-- local x = u > f
-- local x = u > coroutine.create(function () end)
