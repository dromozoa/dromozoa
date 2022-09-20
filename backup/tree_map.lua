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

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = { __name = "dromozoa.tree_map" }

---------------------------------------------------------------------------

function class:insert(k, v)
  if k == nil then
    error "key is nil"
  elseif type(k) == "number" and k ~= k then
    error "key is NaN"
  elseif v == nil then
    error "value is nil"
  end
  local priv = private[self]
  local V = priv.V
  local inserted, i = priv:insert(k)
  if inserted then
    V[i] = v
  end
  return self, V[i], inserted
end

function class:assign(k, v)
  if k == nil then
    error "key is nil"
  elseif type(k) == "number" and k ~= k then
    error "key is NaN"
  elseif v == nil then
    error "value is nil"
  end
  local priv = private[self]
  local V = priv.V
  local inserted, i = priv:insert(k)
  V[i] = v
  return self, v, inserted
end

function class:insert_or_update(k, insert_fn, update_fn)
  if k == nil then
    error "key is nil"
  elseif type(k) == "number" and k ~= k then
    error "key is NaN"
  end
  local priv = private[self]
  local V = priv.V
  local inserted, i = priv:insert(k)
  local v = V[i]
  if inserted then
    v = insert_fn()
  elseif update_fn ~= nil then
    v = update_fn(v)
  end
  if v == nil then
    error "value is nil"
  end
  V[i] = v
  return self, v, inserted
end

function class:find(k)
  return select(2, private[self]:find(k))
end

function class:each(lower_bound, upper_bound)
  return coroutine.wrap(function (self)
    for k, v in self:each(lower_bound, upper_bound) do
      coroutine.yield(k, v)
    end
  end), private[self]
end

---------------------------------------------------------------------------

function class:empty()
  return private[self].size == 0
end

function class:pairs()
  return coroutine.wrap(function (self)
    for i, k in ipairs(self.K) do
      coroutine.yield(k, self.V[i])
    end
  end), private[self]
end

---------------------------------------------------------------------------

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
  if k == "compare" then
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

---------------------------------------------------------------------------

return setmetatable(class, {
  __call = function (_, compare)
    local self = setmetatable({}, metatable)
    private[self] = tree(compare)
    return self
  end;
})
