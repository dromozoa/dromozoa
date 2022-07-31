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

local function tree_map(compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end

function metatable:__index(k)
  local _, v = private[self]:find(k)
  return v
end

function metatable:__newindex(k, v)
  if k == nil then
    error "table index is nil"
  elseif type(k) == "number" and k ~= k then
    error "table index is NaN"
  end

  if v == nil then
    private[self]:delete(k)
  else
    private[self]:insert(k, v)
  end
end

function metatable:__call(k, fn)
  if k == nil then
    return private[self]
  end

  if fn == nil then
    fn = function () return tree_map(private[self].compare) end
  end
  local _, v = private[self]:insert(k, nil, fn)
  return v
end

function metatable:__pairs()
  error "not supported"
  -- return tree.next, private[self], nil
end

return tree_map
