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

-- insertedかどうかを調べる必要はある？
-- TODO ゆくゆくは削除もできるようにする
-- TODO 上書きするのはよいのか？
function class:insert(k, v)
  assert(k ~= nil)
  assert(v ~= nil)
  local ok, _, i = private[self]:insert(k, v)
  return self, i, ok
end

-- insert(k, v)
-- assign(k, v)

-- TODO SQL的なインターフェース
-- insert ... where key = :key
-- update ... where key = :key
--
-- insert(key, fn)
-- insert_or_update(key, fn, fn)
-- update(key, fn)
-- 成功したら、valueを返す
--   assert(insert(key, fn))
--
--
--

-- put
-- set / assign
-- get(key, fn)


class.insert_or_assign = class.insert
class.assign = class.insert

-- function class:insert_or_assign(k, v)
--   if k == nil then
--     error "table index is nil"
--   elseif type(k) == "number" and k ~= k then
--     error "table index is NaN"
--   elseif v == nil then
--     -- 削除はできない
--     error "table value is nil"
--   end
--   local ok, _, i = private[self]:insert(k, v)
--   return self, i, ok
-- end

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

-- i,k,v
-- function class:ipairs

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
  end, private[self], nil
end

-- TODO tree_eachを実装する

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
  local v = class[k]
  if v ~= nil then
    return v
  end
  -- { K[i], V[i] }を返す？
  error "not supported"
end

function metatable:__newindex()
  error "not supported"
end

function metatable:__pairs()
  error "not supported"
end

metatable["dromozoa.stable_pairs"] = function (self)
  return private[self]:each()
end

return function (compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end
