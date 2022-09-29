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

local function f1()
end

local function f2(a)
end

local function f3(...)
end

local function f1()
  return
end

local function f2(a)
  return
end

local function f3(...)
  return
end

local function f1()
  return 1
end

local function f2(a)
  return 2
end

local function f3(...)
  return 3
end

local function f1()
  return 1
end

local function f2(a)
  return 2, a
end

local function f3(...)
  return 3, ...
end

local function f1()
  local f <close> = io.open "/dev/null"
  return 1
end

local function f2(a)
  local f <close> = io.open "/dev/null"
  return 2
end

local function f3(...)
  local f <close> = io.open "/dev/null"
  return 3
end
