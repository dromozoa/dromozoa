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

local v = 0
local function f()
  v = v * 2 + 1
  return v
end
local t = {[0]={}}

local g = function ()
  local z = 0
  t[z][f()], t[z][f()], t[z][f()] = f(), f(), f()
  t[f()], t[f()], t[f()] = f(), f(), f()
end

g()
print(1, t[1]) -- 15
print(3, t[3]) -- 31
print(7, t[7]) -- 63
print("v", v)  -- 63
