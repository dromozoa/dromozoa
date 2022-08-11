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

local a = 1
local a = a + a
local a = a + a

do
  local a = a + a
end

do
  local a = a + a
  do
    local a = a + a
  end
end

do
  local a = a + a
  do
    local a = a + a
    do
      local a = a + a
    end
  end
end

local i = 1
local j = 2
local k = 3

for i = i, j do end
for i = i, j, k do end

local t = {}
local f = next
local k = nil
local c = nil

for k, v in t, f, k, c do end

repeat
  local x
until not x

while false do
  local y
end

local a = 1
local b = 2
local c <const> = 3

local function f1()
  local function f2()
    local function r3()
      b = c
      a = b
      print(a,b,c)
    end
  end
end



