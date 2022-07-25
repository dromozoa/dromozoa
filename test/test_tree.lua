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

---------------------------------------------------------------------------

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
    local V = self.V
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
      V[deleted] = V[t]
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

---------------------------------------------------------------------------

local function dispose(self, t)
  local K = self.K
  local V = self.V
  local L = self.L
  local R = self.R
  local N = self.N
  local comp = self.comp

  local u = self.size
  self.size = u - 1

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
        else
          v = R[v]
        end
      end
    end

    K[t] = K[u]
    V[t] = V[u]
    L[t] = L[u]
    R[t] = R[u]
    N[t] = N[u]
  end

  K[u] = nil
  V[u] = nil
  L[u] = nil
  R[u] = nil
  N[u] = nil
end

local function find(self, x)
  local K = self.K
  local V = self.V
  local L = self.L
  local R = self.R
  local comp = self.comp

  local t = self.root
  while t ~= 0 do
    if comp(x, K[t]) then
      t = L[t]
    elseif comp(K[t], x) then
      t = R[t]
    else
      return K[t], V[t]
    end
  end
end

---------------------------------------------------------------------------

local function next(self, x, t, k, v)
  if t ~= 0 then
    local K = self.K
    local V = self.V
    local L = self.L
    local R = self.R
    local comp = self.comp

    if x == nil or comp(x, K[t]) then
      k, v = next(self, x, L[t], k, v)
      if k == nil then
        return K[t], V[t]
      else
        return k, v
      end
    end
    return next(self, x, R[t], k, v)
  end

  return k, v
end

local function each(self, x, y, t)
  if t ~= 0 then
    local K = self.K
    local V = self.V
    local L = self.L
    local R = self.R
    local comp = self.comp

    if y == nil or comp(K[t], y) then
      if x == nil or comp(x, K[t]) then
        each(self, x, y, L[t])
        coroutine.yield(K[t], V[t])
      elseif not comp(K[t], x) then
        coroutine.yield(K[t], V[t])
      end
      return each(self, x, y, R[t])
    end
  end
end

---------------------------------------------------------------------------

function class:insert(key, value)
  local root, ok, last = insert(self, key, self.root, false, 0)
  self.root = root
  self.V[last] = value
  return ok
end

function class:delete(key)
  local root, ok, last = delete(self, key, self.root, false, 0, 0)
  self.root = root
  if ok then
    dispose(self, last)
  end
  return ok
end

function class:find(key)
  return find(self, key)
end

function class:next(key)
  return next(self, key, self.root)
end

function class:each(lower_bound, upper_bound)
  return coroutine.wrap(function (self, t)
    return each(self, lower_bound, upper_bound, t)
  end), self, self.root
end

---------------------------------------------------------------------------

local function tree(comp)
  if comp == nil then
    comp = function (a, b) return a < b end
  end

  return setmetatable({
    K = {};
    V = {};
    L = { [0] = 0 };
    R = { [0] = 0 };
    N = { [0] = 0 };
    root = 0;
    size = 0;
    comp = comp;
  }, metatable)
end

---------------------------------------------------------------------------

local metatable = { __index = class, __name = "dromozoa.tree.map" }

local private = { __mode = "k" }

local function tree_map(comp)
  local self = setmetatable({}, metatable)
  private[self] = tree(comp)
  return self
end

function metatable:__index(key)
  local _, value = private[self]:find(key)
  return value
end

function metatable:__newindex(key, value)
  if value == nil then
    private[self]:delete(key)
  else
    private[self]:insert(key, value)
  end
end

function metatable:__call(key, fn)
  local value = self[key]
  if value == nil then
    if fn == nil then
      value = tree_map()
    else
      value = fn()
    end
    self[key] = value
  end
  return value
end

function metatable:__pairs()
  return class.next, private[self]
end

---------------------------------------------------------------------------

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
  -- io.write(("  "):rep(n), k, " ", tostring(self.K[t]), "=", tostring(self.V[t]), "\n")
  if self.L[t] ~= 0 then
    dump(self, self.L[t], n, "L")
  end
  if self.R[t] ~= 0 then
    dump(self, self.R[t], n, "R")
  end
end

local self = tree()
for i = 1, 16 do
  self:insert(i, i*2)
end
dump(self, self.root)

io.write "----\n"

local buffer = {}
for k, v in self:each() do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

self:insert(3, 6)

self:delete(3)
dump(self, self.root)

self:delete(3)

assert(self:find(1))
assert(self:find(2))
assert(not self:find(3))
assert(self:find(4))

local buffer = {}
for k, v in self:each() do
  buffer[#buffer + 1] = k
end
assert(table.concat(buffer, ",") == "1,2,4,5,6,7,8,9,10,11,12,13,14,15,16")


local buffer = {}
for k, v in self:each(10) do
  buffer[#buffer + 1] = k
  assert(v == k*2)
end
assert(table.concat(buffer, ",") == "10,11,12,13,14,15,16")

local buffer = {}
for k, v in self:each(10.5) do
  buffer[#buffer + 1] = k
  assert(v == k*2)
end
assert(table.concat(buffer, ",") == "11,12,13,14,15,16")

local buffer = {}
for k, v in self:each(10, 15) do
  buffer[#buffer + 1] = k
  assert(v == k*2)
end
assert(table.concat(buffer, ",") == "10,11,12,13,14")

local buffer = {}
for k, v in self:each(10.5, 15.5) do
  buffer[#buffer + 1] = k
  assert(v == k*2)
end
assert(table.concat(buffer, ",") == "11,12,13,14,15")

assert(self:next(nil) == 1)
assert(self:next(0) == 1)
assert(self:next(1) == 2)
assert(self:next(2) == 4)
assert(self:next(3) == 4)
assert(self:next(4) == 5)
assert(self:next(10) == 11)
assert(self:next(10.5) == 11)
assert(self:next(11) == 12)
assert(self:next(15) == 16)
assert(self:next(16) == nil)
assert(self:next(17) == nil)

for k, v in class.next, self do
  assert(v == k*2)
end

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
  self:insert(i, i*3)
end
dump(self, self.root)

io.write "----\n"

self:delete(3)
dump(self, self.root)

for i = 1, 16 do
  if i ~= 3 then
    self:delete(i)
  end
end
assert(self.root == 0)

---------------------------------------------------------------------------

local m = tree_map()

m.foo = 1
m.bar = 2
m.baz = 3
m.qux = 4
assert(private[m].size == 4)

m.bar = nil
assert(private[m].size == 3)

assert(m.foo == 1)
assert(m.bar == nil)
assert(m.baz == 3)
assert(m.qux == 4)

local buffer = {}
for k, v in pairs(m) do
  buffer[#buffer + 1] = k .. "=" .. v
end
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")

local buffer = {}
for k, v in pairs(m) do
  buffer[#buffer + 1] = k .. "=" .. v
  m[k] = nil
end
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")
