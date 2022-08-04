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

local data = {}
for byte = 0x00, 0xFF do
  data[#data + 1] = byte
end

-- Lua 5.1だけエスケープの方式が異なる
--   E = '\\'
--   0x00 => E 000
--   0x0A => E \n
--   0x0D => E r
--   0x22 => E "
--   0x5C => E E
-- LuaJITとLua 5.2以降はおなじ
--   E = '\\'
--   0x00..0x09 => E \d{1} or E \d{3} 数字が後続するときは3桁
--   0x0A       => E \n
--   0x0B..0x1F => E \d{2} or E \d{3} 数字が後続するときは3桁
--   0x22 => E "
--   0x5C => E E
--   0x7F       => E 127
-- つまり、0x00..0x0A, 0x0B..0x1F, 0x7Fで出力が異なる。

local q = string.format("%q", string.char((table.unpack or unpack)(data)))
-- io.write(q)

local q = string.format("%q", "\0011\0111")
-- io.write(q)

