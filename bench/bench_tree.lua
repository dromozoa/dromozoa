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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local tree_map = require "dromozoa.tree_map"

local N = 100000

local data = {}
for i = 1, N do
  -- data[i] = i .. ("x"):rep(i % 256 + 40) .. i
  data[i] = i
end

local t = tree_map()
-- local t = {}

math.randomseed(42)
for i = 1, N - 1 do
  local j = math.random(i, N)
  data[i], data[j] = data[j], data[i]
end

local function pairs(t)
  local metatable = getmetatable(t)
  if metatable then
    local metamethod = metatable.__pairs
    if metamethod then
      return metamethod(t)
    end
  end
  return next, t
end

collectgarbage()
collectgarbage()
local m1 = collectgarbage "count"

local t1 = os.clock()
for i = 1, N do
  local k = data[i]
  t[k] = k
end
local t2 = os.clock()

collectgarbage()
collectgarbage()
local m2 = collectgarbage "count"

io.write(("clock:  %9.3f\n"):format(t2 - t1))
io.write(("memory: %9.3f\n"):format(m2 - m1))

local t1 = os.clock()
local x = 0
for k, v in pairs(t) do
  x = x + 1
end
local t2 = os.clock()

io.write(("clock:  %9.3f\n"):format(t2 - t1))

local each

if getmetatable(t) then
  each = function (t)
    return t():each()
  end
else
  each = function (t)
    return next, t
  end
end

local t1 = os.clock()
local x = 0
for k, v in each(t) do
  x = x + 1
end
local t2 = os.clock()

io.write(("clock:  %9.3f\n"):format(t2 - t1))

collectgarbage()
collectgarbage()
local m1 = collectgarbage "count"

local t1 = os.clock()
local x = 0
for k, v in pairs(t) do
  x = x + 1
  if x % 2 == 1 then
    t[k] = nil
  end
end
local t2 = os.clock()

collectgarbage()
collectgarbage()
local m2 = collectgarbage "count"

io.write(("clock:  %9.3f\n"):format(t2 - t1))
io.write(("memory: %9.3f\n"):format(m2 - m1))
print(m1, m2)
