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

local metatable = {
  __close = function (self)
    print("__close", self, self.v)
    self.v = 0
  end;
}

local function tbc(v)
  local self = setmetatable({v=v}, metatable)
  print("tbc", self, self.v)
  return self
end

local v = 0

local a <close> = tbc(100)
do
  local b <close> = tbc(200)
  ::L1::
  local c <close> = tbc(v)

  if v == 0 then
    local d <close> = tbc(300)
    v = 1
    goto L1
    local e <close> = tbc(400)
  else
    local f <close> = tbc(500)
    goto L2
  end

  -- lua: label_tbc.lua:49: <goto L2> at line 44 jumps into the scope of local 'e'
  local g <close> = tbc(600)
  ::L2::
  ::L3::
  -- local h <close> = tbc(700)
end
