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

local tree = require "dromozoa.tree"

local metatable = { __name = "dromozoa.tree_map" }
local private = setmetatable({}, { __mode = "k" })

local function class(compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end

function metatable:__index(k)
  if k == nil then
    return nil
  end

  local _, v = private[self]:find(k)
  return v
end

function metatable:__newindex(k, v)
  if k == nil then
    error "table index is nil"
  end

  if v == nil then
    private[self]:delete(k)
  else
    private[self]:insert(k, v)
  end
end

function metatable:__call(k, constructor)
  if k == nil then
    return private[self]
  end

  local v = self[k]
  if v == nil then
    if constructor == nil then
      constructor = class
    end
    v = constructor(private[self].compare)
    self[k] = v
  end
  return v
end

function metatable:__pairs()
  return tree.next, private[self]
end

return class
