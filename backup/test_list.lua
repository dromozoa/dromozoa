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
assert(compare(list(1,2,3,4,5):append(6,7,8,9), {1,2,3,4,5,6,7,8,9}) == 0)

for _, t in ipairs { list(1,2,3,4,5) } do
  local u = {}
  for k, v in pairs(t) do
    u[k] = v
  end

  for i = 0, 5 do
    if i == 0 then
      i = nil
    end
    for j = 0, 5 do
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
