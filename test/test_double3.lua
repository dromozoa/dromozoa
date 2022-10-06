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

local function hex(b)
  if b <= 0x39 then
    return b - 0x30
  elseif b <= 0x46 then
    return b - 0x41 + 10
  else
    return b - 0x61 + 10
  end
end

local function f(s)
  -- 16文字

  local x = tonumber(s)

  local p = 0
  local s = s:gsub("[Pp]([%+%-]?%d+)$", function (v)
    p = tonumber(v)
    return ""
  end)
  local s = s:gsub("%.([0-9A-Fa-f]*)$", function (v)
    p = p - #v * 4
    return v
  end)
  local s = s:gsub("^0[Xx]0*", "")
  local n = #s
  if n >= 16 then
    p = p + (n - 16) * 4
    s = s:sub(1, 16)
  else
    p = p - (16 - n) * 4
    s = s .. ("0"):rep(16 - n)
  end

  local v = math.ldexp(math.ldexp(tonumber("0x" .. s:sub(1, 8)), 32) + tonumber("0x" .. s:sub(9, 16)), p)
  assert(v == x)
end

f "0x00123.450"
f "0x00123.450p1"
f "0x00123.450p+1"
f "0x00123.450p-1"
f "0x00.012345"
f "0x00.012345p1"
f "0x00.012345p+1"
f "0x00.012345p-1"

for i = 1, 32 do
  local z = ("00000000"):rep(i)
  local s = "0x" .. z .. "1.23456789abcdef" .. z .. "deadbeeffeedfacep+1023"
  f(s)
end
