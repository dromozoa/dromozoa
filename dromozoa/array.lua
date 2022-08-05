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

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = { __index = class, __name = "dromozoa.array" }
local table_unpack = table.unpack or unpack

---------------------------------------------------------------------------

local function construct(priv, check)
  if check ~= nil then
    for i = 1, check do
      if priv[i] == nil then
        error "value is nil"
      end
    end
  end

  local self = setmetatable({}, metatable)
  private[self] = priv
  return self
end

---------------------------------------------------------------------------

function class:append(...)
  local priv = private[self]
  local n = #priv
  for i = 1, select("#", ...) do
    local v = select(i, ...)
    if v == nil then
      error "value is nil"
    end
    priv[n + i] = v
  end
  return self
end

function class:set(i, v)
  if i == nil then
    error "index is nil"
  elseif type(i) == "number" and i ~= i then
    error "index is NaN"
  elseif v == nil then
    error "value is nil"
  end
  local priv = private[self]
  if priv[i] == nil then
    error "out of range"
  end
  priv[i] = v
  return self
end

function class:slice(i, j)
  local priv = private[self]
  if i == nil then
    i = 1
  elseif i < 1 then
    error "value is nil"
  end
  if j == nil then
    j = #priv
  elseif i <= j and j > #priv then
    error "value is nil"
  end
  return construct { table_unpack(priv, i, j) }
end

---------------------------------------------------------------------------

function class:empty()
  return next(private[self]) == nil
end

function class:size()
  return #private[self]
end

function class:get(i)
  return private[self][i]
end

function class:ipairs()
  return ipairs(private[self])
end

function class:concat(...)
  return table.concat(private[self], ...)
end

function class:sort(...)
  return table.sort(private[self], ...)
end

function class:unpack(...)
  return table_unpack(private[self], ...)
end

---------------------------------------------------------------------------

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
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

local module = {}

function module.fill(n, v)
  if v == nil then
    error "value is nil"
  end
  local priv = {}
  for i = 1, n do
    priv[i] = v
  end
  return construct(priv)
end

return setmetatable(module, {
  __index = class;
  __call = function (_, ...)
    return construct({...}, select("#", ...))
  end
})
