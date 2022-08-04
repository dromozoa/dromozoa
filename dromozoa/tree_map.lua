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
local tree = require "dromozoa.tree"

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = {
  __name = "dromozoa.tree_map";
  ["dromozoa.stable_pairs"] = function (self) return private[self]:each() end;
}

function class:insert(k, v)
  assert(k ~= nil)
  assert(v ~= nil)
  local ok, _, i = private[self]:insert(k, v)
  return self, i, ok
end

-- TODO SQL的なインターフェース
-- insert ... where key = :key
-- update ... where key = :key

-- put
-- set / assign
-- get(key, fn)

class.insert_or_assign = class.insert
class.assign = class.insert

-- TODO getはfindにするべき？
function class:get(k, fn)
  if fn == nil then
    local _, v = private[self]:find(k)
    return v
  else
    local _, v = private[self]:insert(k, nil, fn)
    return v
  end
end

function class:empty()
  return private[self].size == 0
end

function class:pairs()
  local i = 0
  return function (self)
    i = i + 1
    local k = self.K[i]
    if k == nil then
      return
    else
      return self.K[i], self.V[i]
    end
  end, private[self]
end

function class:tree_each(lower_bound, upper_bound)
  return coroutine.wrap(function (self)
    for k, v in self:each(lower_bound, upper_bound) do
      coroutine.yield(k, v)
    end
  end), private[self]
end

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
  if k == "tree_compare" then
    return private[self].compare
  end
  local v = class[k]
  if v ~= nil then
    return v
  end
  error "not supported"
end

function metatable:__newindex()
  error "not supported"
end

function metatable:__pairs()
  error "not supported"
end

function metatable:__tostring()
  error "not supported"
end

metatable["dromozoa.stable_pairs"] = function (self)
  return private[self]:each()
end

return setmetatable(class, {
  __call = function (_, compare)
    local self = setmetatable({}, metatable)
    private[self] = tree(compare)
    return self
  end;
})
