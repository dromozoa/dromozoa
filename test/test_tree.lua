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

function class:skew(u)
  local L = self.L
  local R = self.R
  local N = self.N

  if N[L[u]] == N[u] then
    u, L[u], R[L[u]] = L[u], R[L[u]], u
  end
  return u
end

function class:split(u)
  local L = self.L
  local R = self.R
  local N = self.N

  if N[R[R[u]]] == N[u] then
    u, R[u], L[R[u]] = R[u], L[R[u]], u
    N[u] = N[u] + 1
  end
  return u
end

function class:insert(x, t, ok)
  local L = self.L
  local R = self.R
  local N = self.N
  local K = self.K

  if t == 0 then
    -- t = #L + 1
    t = (self.n or 0) + 1
    self.n = t
    L[t] = 0
    R[t] = 0
    N[t] = 1
    K[t] = x
    ok = true
  else
    if x < K[t] then
      L[t], ok = self:insert(x, L[t])
    elseif x > K[t] then
      R[t], ok = self:insert(x, R[t])
    else
      ok = false
    end
    -- inline skew

    -- t = self:skew(t)
    if N[L[t]] == N[t] then
      t, L[t], R[L[t]] = L[t], R[L[t]], t
    end

    -- t = self:split(t)
    if N[R[R[t]]] == N[t] then
      t, R[t], L[R[t]] = R[t], L[R[t]], t
      N[t] = N[t] + 1
    end

  end
  return t, ok
end

function class:delete(x, t, ok, last, deleted)
  ok = false
  if t ~= 0 then
    local L = self.L
    local R = self.R
    local N = self.N
    local K = self.K

    -- 1. Search down the tree and set pointers last and deleted.
    last = t
    if x < K[t] then
      L[t], ok, last, deleted = self:delete(x, L[t], ok, last, deleted)
    else
      deleted = t
      R[t], ok, last, deleted = self:delete(x, R[t], ok, last, deleted)
    end

    -- 2. At the bottom of the tree we remove the element (if it is present).
    if t == last and deleted ~= nil and x == K[deleted] then
      -- 削除対象のキーを葉ノードのキーにおきかえてのっとる
      K[deleted] = K[t]
      deleted = nil
      -- t = t.right -- 右にノードがいる場合がある
      -- print("removed: ", t)
      local u = t
      t = R[t]
      last = nil

      L[u] = nil
      R[u] = nil
      K[u] = nil
      N[u] = nil
      -- この位置におしりのをつっこんできれいにする

      -- tは消されて、t.rightがその位置におさまる

      ok = true

    -- 3. On the way back, we rebalance.
    else
      if N[L[t]] < N[t] - 1 or N[R[t]] < N[t] - 1 then
        N[t] = N[t] - 1
        if N[R[t]] > N[t] then
          N[R[t]] = N[t]
        end
        t = self:skew(t)
        R[t] = self:skew(R[t])
        R[R[t]] = self:skew(R[R[t]])
        t = self:split(t)
        R[t] = self:split(R[t])
      end
    end
  end
  return t, ok, last, deleted
end

function class:find(x, t)
  local L = self.L
  local R = self.R
  local K = self.K

  while t ~= 0 do
    if x == K[t] then
      return t
    elseif x < K[t] then
      t = L[t]
    else
      t = R[t]
    end
  end
end

local function each(self, t)
  local L = self.L
  local R = self.R
  local K = self.K

  if t ~= 0 then
    each(self, L[t])
    coroutine.yield(K[t])
    each(self, R[t])
  end
end

function class:each(t)
  return coroutine.wrap(function (self)
    each(self, t)
  end), self
end

local function tree()
  return setmetatable({
    L = { [0] = 0 };
    R = { [0] = 0 };
    N = { [0] = 0 };
    K = {};
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
local u = 0
local v
local ok = nil
for i = 1, 16 do
  u, ok = self:insert(i, u)
  assert(ok)
end
dump(self, u)

io.write "----\n"

local buffer = {}
for k in self:each(u) do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

v, ok = self:insert(3, u)
assert(not ok)
assert(u == v)

u, ok = self:delete(3, u)
assert(ok)
dump(self, u)

v, ok = self:delete(3, u)
assert(not ok)
assert(u == v)

assert(self:find(1, u))
assert(self:find(2, u))
assert(not self:find(3, u))
assert(self:find(4, u))

local buffer = {}
for k in self:each(u) do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,4,5,6,7,8,9,10,11,12,13,14,15,16")

-- print(dumper.encode(self, { stable = true, pretty = true }))

for i = 1, 16 do
  if i ~= 3 then
    u, ok = self:delete(i, u)
    assert(ok)
  end
end
assert(u == 0)

io.write "====\n"

local self = tree()
local u = 0
for i = 16, 1, -1 do
  u, ok = self:insert(i, u)
  assert(ok)
end
dump(self, u)

io.write "----\n"

u, ok = self:delete(3, u)
assert(ok)
dump(self, u)

for i = 1, 16 do
  if i ~= 3 then
    u, ok = self:delete(i, u)
    assert(ok)
  end
end
assert(u == 0)
