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

local a, b <const>, c = 1, 2, 3
local h <close> = io.open "/dev/null"

function f()
  function g ()
    a = 42
    -- b = 69
    -- h = io.open "/dev/null"
  end
end

do
  -- local h1 <close>, h2 <close>
  -- lua: attribute.lua:32: multiple to-be-closed variables in local list
  a = 69
  a = 42
end

do
  local h1 <close>, h2 <const>
  local h3 <const>, h4 <close>
  local h5 <const>, h6 <const>
end

local x = 1
repeat
  local c1 <close>
  x = 2
  if x == 3 then
    local c2 <close>
    break
  end
  local c3 <close>
  x = 3
until false

-- local nsa <no_such_attribute> = 0
-- lua: attribute.lua:54: unknown attribute 'no_such_attribute'
