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

local _ = 3, 345, 0xff, 0xBEBADA
local _ = 3.0, 3.1416, 314.16e-2, 0.31416E1, 34e1, 0x0.1E, 0xA23p-4, 0X1.921FB54442D18P+1
local _ = - 65536, - 0x10001, - 65538.0
local _ = - - 65539, - - 0x10004, - - 65541.0
local _ =  9223372036854775808
local _ = -9223372036854775808
local _ =  9223372036854775807+1
local _ = -9223372036854775807-1

-- 0xFEDCBA9876543210に等しい
local _ = 0x10FEDCBA9876543210
