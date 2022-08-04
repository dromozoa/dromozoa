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

---------------------------------------------------------------------------
-- eachはO(1)が保証されるけれど、途中でコンテナを変更すると、危険かもしれない
-- pairsはO(log n)が保証されていて、途中でコンテナを変更しても安全
-- ipairsはどうしよう？
--
-- TODO 最終的には、tree_set/tree_mapと名乗る
-- ipairs, pairsは、定義通りの意味をとるべき。
-- index orderとtree orderのふたつのeachを用意する
-- そのほかに、編集安全なeachも用意する
-- indexじゃなくて、handle/pointerと呼ぶ案
-- index/handle/pointerは、deleteが行われるまで使うことができる
-- indexのK,Vはtreeのものをつかえる
--
-- pairsは、handle順を返す
-- eachは、tree順を返す、レンジ指定子もつけられる
--
--
---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = { __name = "dromozoa.tree_set" }
local table_unpack = table.unpack or unpack

function class:empty()
  return private[self].size == 0
end

function class:size()
  return private[self].size
end

function class:get(i)
  return private[self].K[i]
end

function class:insert(k)
  if k == nil then
    error "table index is nil"
  elseif type(k) == "number" and k ~= k then
    error "table index is NaN"
  end
  local ok, _, i = private[self]:insert(k)
  return self, i, ok
end

function class:ipairs()
  return ipairs(private[self].K)
end

function class:concat(...)
  return table.concat(private[self].K, ...)
end

function class:unpack(...)
  return table_unpack(private[self].K, ...)
end

function class:tree_each(lower_bound, upper_bound)
  return coroutine.wrap(function (self)
    for k, _, i in self:each(lower_bound, upper_bound) do
      coroutine.yield(i, k)
    end
  end), private[self]
end

function metatable:__len()
  error "not supported"
end

-- TODO 順序を再考する
function metatable:__index(k)
  local v = class[k]
  if v ~= nil then
    return v
  end
  if k == "tree_compare" then
    return private[self].compare
  end

  local v = private[self].K[k]
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

return setmetatable(class, {
  __call = function (_, compare)
    local self = setmetatable({}, metatable)
    private[self] = tree(compare)
    return self
  end;
})
