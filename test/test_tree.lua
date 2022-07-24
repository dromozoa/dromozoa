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

function class:skew(t)
  local L = self.L
  local N = self.N

  if N[L[t]] == N[t] then
    local R = self.R
    t, L[t], R[L[t]] = L[t], R[L[t]], t
  end
  return t
end

function class:split(t)
  local R = self.R
  local N = self.N

  if N[R[R[t]]] == N[t] then
    local L = self.L
    t, R[t], L[R[t]] = R[t], L[R[t]], t
    N[t] = N[t] + 1
  end
  return t
end

function class:insert(x, t)
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
  else
    if x < K[t] then
      L[t] = self:insert(x, L[t])
    elseif x > K[t] then
      R[t] = self:insert(x, R[t])
    else
      error "key exists"
    end
    t = self:skew(t)
    t = self:split(t)
  end
  return t
end

function class:delete(x, t, last, deleted)
  if t ~= 0 then
    local L = self.L
    local R = self.R
    local N = self.N
    local K = self.K

    -- 1. Search down the tree and set pointers last and deleted.
    last = t
    if x < K[t] then
      L[t], last, deleted = self:delete(x, L[t], last, deleted)
    else
      deleted = t
      R[t], last, deleted = self:delete(x, R[t], last, deleted)
    end

    -- 2. At the bottom of the tree we remove the element (if it is present).
    if t == last and deleted ~= nil and x == K[deleted] then
      -- 削除対象のキーを葉ノードのキーにおきかえてのっとる
      K[deleted] = K[t]
      deleted = nil
      -- t = t.right -- 右にノードがいる場合がある
      -- print("removed: ", t)
      t = R[t]
      last = nil
      -- tは消されて、t.rightがその位置におさまる

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
  return t, last, deleted
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
for i = 1, 16 do
  u = self:insert(i, u)
end
dump(self, u)

io.write "----\n"

u = self:delete(3, u)
dump(self, u)

for i = 1, 16 do
  if i ~= 3 then
    u = self:delete(i, u)
  end
end
assert(u == 0)

io.write "====\n"

local self = tree()
local u = 0
for i = 16, 1, -1 do
  u = self:insert(i, u)
end
dump(self, u)

io.write "----\n"

u = self:delete(3, u)
dump(self, u)

for i = 1, 16 do
  if i ~= 3 then
    u = self:delete(i, u)
  end
end
assert(u == 0)
