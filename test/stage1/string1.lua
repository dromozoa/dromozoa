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

local s = "あいうえお"
print(string.len(s))
print(s:len())
print(#s)

print(s:byte())
print(s:byte(1))
print(s:byte(1,2))
print(s:byte(1,-1))
print(s:byte(1,-32))

print(string.char())
print(string.char(0xE3, 0x82, 0xAB, 0xE3, 0x82, 0xAD, 0xE3, 0x82, 0xAF, 0xE3, 0x82, 0xB1, 0xE3, 0x82, 0xB3))

print(s:sub(1))
print(s:sub(4))
print(s:sub(4, 9))
