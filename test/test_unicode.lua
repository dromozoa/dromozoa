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

local append = require "dromozoa.append"

local data = {
  -- RFC 3629
  0x0041, 0x2262, 0x0391, 0x002E;
  0xD55C, 0xAD6D, 0xC5B4;
  0x65E5, 0x672C, 0x8A9E;
  0x233B4;

  0x0000007F;
  0x00000080;
  0x000007FF;
  0x00000800;
  0x0000FFFF;
  0x00010000;
  0x0010FFFF;
  0x00110000;
  0x7FFFFFFF;
  -- 0x80000000;
}

if verbose and utf8 ~= nil and utf8.char ~= nil then
  for _, c in ipairs(data) do
    print(string.byte(utf8.char(c), 1, -1))
  end
end

local function append_utf8(a, buffer)
  --                 0x7F 0111 1111        7F >>  0 = 0x7F
  -- 0xC0 1100 0000  0x1F 0001 1111       7FF >>  6 = 0x1F
  -- 0xE0 1110 0000  0x0F 0000 1111      FFFF >> 12 = 0x0F
  -- 0xF0 1111 0000  0x07 0000 0111    1CFFFF >> 18 = 0x07
  -- 0xF8 1111 1000  0x03 0000 0011   3FFFFFF >> 24 = 0x03
  -- 0xFC 1111 1100  0x01 0000 0001  7FFFFFFF >> 30 = 0x01

  local n = #buffer + 1
  if a <= 0x7F then
    buffer[n] = a
  elseif a <= 0x7FF then
    local b = a % 0x40
    buffer[n] = (a - b) / 0x40 + 0xC0
    buffer[n + 1] = b + 0x80
  elseif a <= 0xFFFF then
    local c = a % 0x40 a = (a - c) / 0x40
    local b = a % 0x40
    buffer[n] = (a - b) / 0x40 + 0xE0
    buffer[n + 1] = b + 0x80
    buffer[n + 2] = c + 0x80
  elseif a <= 0x1CFFFF then
    local d = a % 0x40 a = (a - d) / 0x40
    local c = a % 0x40 a = (a - c) / 0x40
    local b = a % 0x40
    buffer[n] = (a - b) / 0x40 + 0xF0
    buffer[n + 1] = b + 0x80
    buffer[n + 2] = c + 0x80
    buffer[n + 3] = d + 0x80
  elseif a <= 0x3FFFFFF then
    local e = a % 0x40 a = (a - e) / 0x40
    local d = a % 0x40 a = (a - d) / 0x40
    local c = a % 0x40 a = (a - c) / 0x40
    local b = a % 0x40
    buffer[n] = (a - b) / 0x40 + 0xF8
    buffer[n + 1] = b + 0x80
    buffer[n + 2] = c + 0x80
    buffer[n + 3] = d + 0x80
    buffer[n + 4] = e + 0x80
  elseif a <= 0x7FFFFFFF then
    local f = a % 0x40 a = (a - f) / 0x40
    local e = a % 0x40 a = (a - e) / 0x40
    local d = a % 0x40 a = (a - d) / 0x40
    local c = a % 0x40 a = (a - c) / 0x40
    local b = a % 0x40
    buffer[n] = (a - b) / 0x40 + 0xFC
    buffer[n + 1] = b + 0x80
    buffer[n + 2] = c + 0x80
    buffer[n + 3] = d + 0x80
    buffer[n + 4] = e + 0x80
    buffer[n + 5] = f + 0x80
  end
end

local buffer = {}
for _, c in ipairs(data) do
  local b = {}
  append_utf8(c, b)
  for i, v in ipairs(b) do
    if i ~= 1 then
      append(buffer, "\t")
    end
    append(buffer, math.floor(v))
  end
  append(buffer, "\n")
end

assert(table.concat(buffer) == [[
65
226	137	162
206	145
46
237	149	156
234	181	173
236	150	180
230	151	165
230	156	172
232	170	158
240	163	142	180
127
194	128
223	191
224	160	128
239	191	191
240	144	128	128
244	143	191	191
244	144	128	128
253	191	191	191	191	191
]])
