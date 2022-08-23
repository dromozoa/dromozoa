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

::L0::

print "ok 1"

goto L1
error "unexpected 1"
::L1::

print "ok 2"

goto L2
do
  ::L2::
  error "unexpected 1"
  goto L0
end
::L2::

print "ok 3"

-- goto L3
-- ::L3::
-- do
--   ::L3::
-- end

goto L4
-- local a = 1
::L4::
local b = 2

-- Lua:    label.lua:47: <goto L4> at line 44 jumps into the scope of local 'a'
-- LuaJIT: label.lua:44: <goto L4> jumps into the scope of local 'a'


do
  local a = 3
  do
    local b = 4
    goto L5
  end

  local c <close> = 5
  ::L5::
  ::L6::
  ;;;;;;
end
print "done"

-- ラベル以降にvoid statementsしかない場合
-- 飛べる:
--   do,while,for,if
-- 飛べない:
--   repeat: 式があるから？
