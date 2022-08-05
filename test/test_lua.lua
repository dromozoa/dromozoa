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

local p_inf = math.huge
local m_inf = -math.huge
local p_zero = 0/p_inf
local m_zero = 0/m_inf
local i_zero = 0
local nan = p_inf/p_inf

if verbose then
  print(([[
       | %-8s | %-8s
-----------------------------
p_inf  | %-8s | %-8s
m_inf  | %-8s | %-8s
p_zero | %-8s | %-8s
m_zero | %-8s | %-8s
i_zero | %-8s | %-8s
nan    | %-8s | %-8s
-----------------------------
p_zero == m_zero | %s
p_zero == i_zero | %s
m_zero == i_zero | %s
nan == nan       | %s
not nan          | %s
not not nan      | %s
nan and 1 or 0   | %s
]]):format(
    "type", "tostring",
    type(p_inf),  tostring(p_inf),
    type(m_inf),  tostring(m_inf),
    type(p_zero), tostring(p_zero),
    type(m_zero), tostring(m_zero),
    type(i_zero), tostring(i_zero),
    type(nan),    tostring(nan),
    tostring(p_zero == m_zero),
    tostring(p_zero == i_zero),
    tostring(m_zero == i_zero),
    tostring(nan == nan),
    tostring(not nan),
    tostring(not not nan),
    tostring(nan and 1 or 0)
    ))

end

local t = { nan }
assert(#t == 1)

local status, message = pcall(function () t[nil] = "nil" end)
if verbose then
  print(message)
end
assert(not status)

local status, message = pcall(function () t[nan] = "nan" end)
if verbose then
  print(message)
end
assert(not status)

local m1 = {
  __eq = function (a, b)
    if verbose then print("m1.__eq", a, b) end
    return false
  end;
  __lt = function (a, b)
    if verbose then print("m1.__lt", a, b) end
    return false
  end;
}

local m2 = {
  __eq = function (a, b)
    if verbose then print("m2.__eq", a, b) end
    return false
  end;
  __lt = function (a, b)
    if verbose then print("m2.__lt", a, b) end
    return false
  end;
}

local x = setmetatable({}, m1)
local y = setmetatable({}, m1)
local z = setmetatable({}, m2)
local f = assert(io.open "/dev/null")

-- LuaJITとLua 5.2までは、左右の型が同じで、かつ同じメタメソッドを持っていなけ
-- ればならない。Lua 5.3以降では、メタメソッドが定義されていれば呼ばれる。

local _ = x == nil
local _ = x == true
local _ = x == 42
local _ = x == "foo"
local _ = x == x -- 等値なので呼ばれない。
local _ = x == y -- メタメソッドが同じなので呼ばれる。
local _ = x == z -- Lua 5.3以降で呼ばれる。
local _ = x == function () end
local _ = x == f
local _ = x == coroutine.create(function () end)

-- LuaJITとLua 5.1では、左右の型が同じで、かつ同じメタメソッドを持っていなけ
-- ればならない。Lua 5.2以降では、メタメソッドが定義されていれば呼ばれる。

local function check(a, b)
  local status, message = pcall(function () return a < b end)
  if not status then
    if verbose then
      print(message, a, b)
    end
  end
end

check(x, nil)
check(x, true)
check(x, 42)
check(x, "foo")
check(x, x)
check(x, y)
check(x, z)
check(x, function () end)
check(x, f)
check(x, coroutine.create(function () end))
