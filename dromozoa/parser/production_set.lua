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

  local k = table.concat(body, ",")
  local n = #self + 1

  local group = self.groups[head]
  if group then
    assert(not group[k])
    group[k] = body
    group[#group + 1] = n
  else
    self.groups[head] = { [k] = body, n }
  end
  self[n] = production
end

function class:each(head)
  return function (group, i)
    i = i + 1
    local j = group[i]
    if j then
      return i, self[j].body
    end
  end, assert(self.groups[head]), 0
end

function class:each_index(head)
  return function (group, i)
    i = i + 1
    local j = group[i]
    if j then
      return i, j
    end
  end, assert(self.groups[head]), 0
end

return function ()
  return setmetatable({ groups = {} }, metatable)
end
