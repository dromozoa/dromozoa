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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local dumper = require "dromozoa.commons.dumper"

local class = {}
local metatable = { __index = class, __name = "dromozoa.tree" }

local function skew(self, t)
  local L = self.L
  local R = self.R
  local N = self.N

  if N[L[t]] == N[t] then
    -- rotate right
    t, L[t], R[L[t]] = L[t], R[L[t]], t
  end

  return t
end

local function split(self, t)
  local L = self.L
  local R = self.R
  local N = self.N

  if N[R[R[t]]] == N[t] then
    -- rotate left
    t, R[t], L[R[t]] = R[t], L[R[t]], t
    N[t] = N[t] + 1
  end

  return t
end

local function insert(self, x, t, ok, last)
  local K = self.K
  local L = self.L
  local R = self.R
  local N = self.N
  local comp = self.comp

  if t == 0 then
    t = self.size + 1
    self.size = t

    K[t] = x
    L[t] = 0
    R[t] = 0
    N[t] = 1

    ok = true
    last = t
  else
    if comp(x, K[t]) then
      L[t], ok, last = insert(self, x, L[t], ok, last)
    elseif comp(K[t], x) then
      R[t], ok, last = insert(self, x, R[t], ok, last)
    else
      ok = false
      last = t
    end
    t = skew(self, t)
    t = split(self, t)
  end

  return t, ok, last
end

local function delete(self, x, t, ok, last, deleted)
  ok = false

  if t ~= 0 then
    local K = self.K
    local L = self.L
    local R = self.R
    local N = self.N
    local comp = self.comp

    -- 1. Search down the tree and set pointers last and deleted.
    last = t
    if comp(x, K[t]) then
      L[t], ok, last, deleted = delete(self, x, L[t], ok, last, deleted)
    else
      deleted = t
      R[t], ok, last, deleted = delete(self, x, R[t], ok, last, deleted)
    end

    -- 2. At the bottom of the tree we remove the element (if it is present).
    if t == last and deleted ~= 0 and not comp(K[deleted], x) then
      K[deleted] = K[t]
      deleted = 0
      t = R[t]
      ok = true

    -- 3. On the way back, we rebalance.
    elseif N[L[t]] < N[t] - 1 or N[R[t]] < N[t] - 1 then
      N[t] = N[t] - 1
      if N[R[t]] > N[t] then
        N[R[t]] = N[t]
      end
      t = skew(self, t)
      R[t] = skew(self, R[t])
      R[R[t]] = skew(self, R[R[t]])
      t = split(self, t)
      R[t] = split(self, R[t])
    end
  end

  return t, ok, last, deleted
end

local function dispose(self, t)
  local K = self.K
  local L = self.L
  local R = self.R
  local N = self.N
  local comp = self.comp

  -- 削除したポインタと最後尾のポインタを入れ替える
  local u = self.size
  self.size = u - 1

  -- 削除したポインタが最後尾のポインタの場合、参照の付け替えは不要
  if t ~= u then
    local v = self.root
    if v == u then
      self.root = t
    else
      while v ~= 0 do
        if L[v] == u then
          L[v] = t
          break
        elseif R[v] == u then
          R[v] = t
          break
        elseif comp(K[u], K[v]) then
          v = L[v]
        elseif comp(K[v], K[u]) then
          v = R[v]
        else
          error "algorithm error"
        end
      end
    end

    K[t] = K[u]
    L[t] = L[u]
    R[t] = R[u]
    N[t] = N[u]
  end

  K[u] = nil
  L[u] = nil
  R[u] = nil
  N[u] = nil

  return u
end

function class:insert(key)
  local root, ok, last = insert(self, key, self.root, false, 0)
  self.root = root
  -- V[last] = v
  return self
end

function class:delete(key)
  local root, ok, last = delete(self, key, self.root, false, 0, 0)
  self.root = root
  if ok then
    local u = dispose(self, last)
    -- V[u] = nil
  end
  return self
end

local function each(self, t)
  local K = self.K
  local L = self.L
  local R = self.R

  if t ~= 0 then
    each(self, L[t])
    coroutine.yield(K[t])
    each(self, R[t])
  end
end

function class:each()
  return coroutine.wrap(each), self, self.root
end

function class:find(x)
  local L = self.L
  local R = self.R
  local K = self.K
  local comp = self.comp

  local t = self.root
  while t ~= 0 do
    if comp(x, K[t]) then
      t = L[t]
    elseif comp(K[t], x) then
      t = R[t]
    else
      return K[t]
    end
  end
end

-- x以上の最小の要素を探して、イテレーションする
local function lower_bound(self, x, t)
  local K = self.K
  local L = self.L
  local R = self.R
  local comp = self.comp

  if t ~= 0 then
    if comp(x, K[t]) then
      lower_bound(self, x, L[t])
      coroutine.yield(K[t])
    elseif not comp(K[t], x) then
      coroutine.yield(K[t])
    end
    lower_bound(self, x, R[t])
  end
end

function class:lower_bound(x)
  return coroutine.wrap(function (self, t)
    lower_bound(self, x, t)
  end), self, self.root
end

local function tree_next(self, x, t, k)
  if t ~= 0 then
    local K = self.K
    local L = self.L
    local R = self.R
    local comp = self.comp

    if comp(x, K[t]) then
      k = tree_next(self, x, L[t], k)
      if k == nil then -- 見つからなかったか、kが等しいものが見つかった
        return K[t]
      else -- kが大きいものが見つかった
        return k
      end
    end
    k = tree_next(self, x, R[t], k)
  end

  return k
end

function class:tree_next(x)
  return tree_next(self, x, self.root)
end

local function tree(comp)
  if comp == nil then
    comp = function (a, b) return a < b end
  end

  return setmetatable({
    K = {};
    L = { [0] = 0 };
    R = { [0] = 0 };
    N = { [0] = 0 };
    root = 0;
    size = 0;
    comp = comp;
  }, metatable)
end

local function dump(self, t, n, k)
  if k == nil then
    k = "/"
  end
  if n == nil then
    n = 0
  else
    n = n + 1
  end
  -- io.write(("  "):rep(n), k, " ", tostring(t), " ", t.level, " / ", tostring(t.key), "\n")
  io.write(("  "):rep(n), k, " ", tostring(self.K[t]), "\n")
  if self.L[t] ~= 0 then
    dump(self, self.L[t], n, "L")
  end
  if self.R[t] ~= 0 then
    dump(self, self.R[t], n, "R")
  end
end

local self = tree()
for i = 1, 16 do
  self:insert(i)
end
dump(self, self.root)

io.write "----\n"

local buffer = {}
for k in self:each() do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

self:insert(3)

self:delete(3)
dump(self, self.root)

self:delete(3)

assert(self:find(1))
assert(self:find(2))
assert(not self:find(3))
assert(self:find(4))

local buffer = {}
for k in self:each() do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,4,5,6,7,8,9,10,11,12,13,14,15,16")


local buffer = {}
for k in self:lower_bound(10) do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "10,11,12,13,14,15,16")

local buffer = {}
for k in self:lower_bound(10.5) do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "11,12,13,14,15,16")

assert(self:tree_next(0) == 1)
assert(self:tree_next(1) == 2)
assert(self:tree_next(2) == 4)
assert(self:tree_next(3) == 4)
assert(self:tree_next(4) == 5)
assert(self:tree_next(10) == 11)
assert(self:tree_next(10.5) == 11)
assert(self:tree_next(11) == 12)
assert(self:tree_next(15) == 16)
assert(self:tree_next(16) == nil)
assert(self:tree_next(17) == nil)

print(dumper.encode(self, { stable = true, pretty = true }))

for i = 1, 16 do
  if i ~= 3 then
    self:delete(i)
  end
end
assert(self.root == 0)

io.write "====\n"

local self = tree()
for i = 16, 1, -1 do
  self:insert(i)
end
dump(self, self.root)

io.write "----\n"

self:delete(3)
dump(self, self.root)

for i = 1, 16 do
  if i ~= 3 then
    self:delete(i, u)
  end
end
assert(self.root == 0)
