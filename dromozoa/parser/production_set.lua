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

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.production_set" }

function class:insert(production)
  local head = production.head
  local body = production.body

  local map = self.map[head]
  if map == nil then
    map = {}
    self.map[head] = map
  end

  -- check
  -- print(#body)
  local key = table.concat(body, ",")
  -- assert(not map[key])
  if not map[key] then
    local n = #self + 1
    self[n] = production
    map[key] = n
    map[#map + 1] = n
  end
end

function class:each_production(head)
  local map = assert(self.map[head])
  local i = 0
  local n = #map

  return function ()
    i = i + 1
    if i <= n then
      local j = map[i]
      return j, self[j].body
    end
  end
end

function class:get(i)
  return self[i]
end

function class:size()
  return #self
end

function class:ipairs()
  return ipairs(self)
end

return function ()
  return setmetatable({ map = {} }, metatable)
end
