-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

---@diagnostic disable: unused-function, unused-local

---@type [integer]
local x

---@type {}
local x

---@type {k:number,[string]:integer}
local x

---@type table
local x

---@type {a:number,[integer]:string, b : boolean , [ string ] : integer; }
local x

---@type fun(a:integer, b:boolean, ...:string):a:integer, ...:string
local x

---@param a integer
---@param b boolean
---@param ... string
---@return integer a
---@return string ...
local function y(a, b, ...)
  return 42, ...
end

x = y

---@type fun(a.b.c:integer):a:integer, x.y.z:integer, あいうえお:integer
local x

---@param abc integer
---@return integer a
---@return integer x.y.z
---@return integer あいうえお
local function y(abc)
  return 1, 2, 3
end

x = y
