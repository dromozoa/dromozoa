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

  local n = #self + 1
  local k = table.concat(body, ",")

  local group = self.groups[head]
  if group then
    assert(not group[k])
    group[k] = n
    group[#group + 1] = n
  else
    self.groups[head] = { [k] = n, n }
  end
  self[n] = production

  -- if group == nil then
  --   group = {}
  --   self.groups[head] = group
  -- end

  -- local body = production.body

  -- -- check
  -- -- print(#body)
  -- local key = table.concat(body, ",")
  -- -- if map[key] then
  -- --   print(head, #body, key)
  -- -- end

  -- assert(not group[key])
  -- if not group[key] then
  --   local n = #self + 1
  --   self[n] = production
  --   group[key] = n
  --   group[#group + 1] = n
  -- end
end

function class:each_production(head)
  local group = assert(self.groups[head])
  local i = 0
  local n = #group

  return function ()
    i = i + 1
    if i <= n then
      local j = group[i]
      return j, self[j].body
    end
  end
end

return function ()
  return setmetatable({ groups = {} }, metatable)
end
