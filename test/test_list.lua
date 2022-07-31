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

local compare = require "dromozoa.compare"
local list = require "dromozoa.list"

local function dump(t)
  local K = {}
  for k, v in pairs(t) do
    K[#K + 1] = k
  end
  table.sort(K, function (a, b) return a < b end)

  local buffer = {}
  for _, k in ipairs(K) do
    buffer[#buffer + 1] = "[" .. k .. "]=" .. t[k]
  end

  return "{" .. table.concat(buffer, ",") .. "}"
end

assert(compare(list(1,2,3,4,5), {1,2,3,4,5}) == 0)
assert(compare(list(1,nil,3,nil,5), {1,nil,3,nil,5}) == 0)
assert(compare(list(nil,2,nil,4,nil), {nil,2,nil,4}) == 0)
assert(compare(list(1,2,3,4,5):append(6,7,8,9), {1,2,3,4,5,6,7,8,9}) == 0)
assert(compare(list(1,nil,3,nil,5):append(6,nil,8,nil), {1,6,nil,8}) == 0)
print(dump(list(nil,2,nil,4,nil):append(nil,6,nil)))

local x = list(nil,2,nil,4,nil)
-- LuaJITでは、#x == 4になった。
assert(#x == 0 or #x == 4)
if #x == 0 then
  assert(compare(x:append(nil,6,nil), {nil,6,nil,4}) == 0)
else
  assert(compare(x:append(nil,6,nil), {nil,2,nil,4,nil,6}) == 0)
  print(#x)
end

for _, t in ipairs { list(1,2,3,4,5), list(1,nil,3,nil,5), list(nil,2,nil,4,nil) } do
  local u = {}
  for k, v in pairs(t) do
    u[k] = v
  end

  for i = -2, 6 do
    if i == -2 then
      i = nil
    end
    for j = -2, 6 do
      if j == -2 then
        j = nil
      end
      local x = t:slice(i, j)
      assert(compare(t, u) == 0)
      local y = { (table.unpack or unpack)(t, i, j) }
      -- print(i, j, dump(x), dump(y))
      assert(compare(x, y) == 0)
    end
  end
end
