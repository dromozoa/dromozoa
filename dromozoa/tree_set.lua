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

local class = {}
local metatable = { __name = "dromozoa.tree_set" }
local private = setmetatable({}, { __mode = "k" })

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
local metatable = {
  __name = "dromozoa.tree_set";
  ["dromozoa.stable_pairs"] = function (self) return private[self]:each() end;
}

-- TODO インターフェースの改良
-- self, handle, insert or updateあたりをかえす
function class:put(k)
  assert(k ~= nil)
  local ok, _, i = private[self]:insert(k)
  return i, ok
end

-- function class:find(k)
--   return private[self]:find(k) ~= nil
-- end

function class:ipairs()
  return ipairs(private[self].K)
end

-- TODO 必要になったら、tree (tree_each) を実装する

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
  if type(k) == "number" and 1 <= k and k <= #private[self].K then
    return private[self].K[k]
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

return function (compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end
