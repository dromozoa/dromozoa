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

--[[

]]

local data = {
  {
    name = "DBL_MAX";
    p = "0x1.FFFFFFFFFFFFFp+1023";
    v = 1.7976931348623157e+308;
    r = "0xFFFFFFFF 0x7FEFFFFF";
  };

  {
    name = "DBL_EPSILON";
    p = "0x1p-52";
    v = 2.2204460492503131e-16;
    r = "0x00000000 0x3CB00000";
  };

  {
    name = "DBL_MIN";
    p = "0x1p-1022";
    v = 2.2250738585072014e-308;
    r = "0x00000000 0x00100000";
  };

  {
    name = "DBL_DEMORM_MIN";
    p = "0x1p-1074";
    v = 4.9406564584124654e-324;
    r = "0x00000001 0x00000000";
  };

  {
    name = "M_PI";
    p = "0x1.921FB54442D18p+1";
    v = math.pi;
    r = "0x54442D18 0x400921FB";
  };

  {
    name = "-M_PI";
    v = -math.pi;
    r = "0x54442D18 0xC00921FB";
  };

  {
    name = "p_inf";
    v = math.huge;
    r = "0x00000000 0x7FF00000";
  };

  {
    name = "m_inf";
    v = -math.huge;
    r = "0x00000000 0xFFF00000";
  };

  {
    name = "p_zero";
    v = 0/math.huge;
    r = "0x00000000 0x00000000";
  };

  {
    name = "m_zero";
    v = 0/-math.huge;
    r = "0x00000000 0x80000000";
  };
}

local function double_to_word(v)
  return string.unpack("<I4I4", string.pack("<d", v))
end

if not string.pack then
  function double_to_word(v)
    assert(v == v)
    if v == 0 then
      if 1 / v > 0 then
        return 0, 0
      else
        return 0, 0x80000000
      end
    elseif v == math.huge then
      return 0, 0x7FF00000
    elseif v == -math.huge then
      return 0, 0xFFF00000
    end

    local a -- 符号    1bit
    local b -- 指数部 11bit
    local c -- 仮数部 20bit+32bit

    if v > 0 then
      a = 0
    else
      a = 0x80000000
      v = -v
    end

    local m, e = math.frexp(v)
    if e <= -1022 then
      b = 0
      c = math.ldexp(m, e + 1022)
    else
      b = e + 1022
      c = (m * 2 - 1)
    end
    local c, d = math.modf(c * 0x100000)

    return d * 0x100000000, a + b * 0x100000 + c
  end
end

for i, item in ipairs(data) do
  local a, b = double_to_word(item.v)
  local s = ("0x%08X 0x%08X"):format(a, b)
  -- print(s)
  assert(s == item.r, item.name .. " " .. s)
end
