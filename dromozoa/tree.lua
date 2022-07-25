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

---------------------------------------------------------------------------

-- Balanced search trees made simple.
-- https://user.it.uu.se/%7Earnea/abs/simp.html

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

    -- TODO refactoring
    if x == nil or comp(x, K[t]) then
      each(self, x, y, L[t])
      if y == nil or comp(K[t], y) then
        coroutine.yield(K[t], V[t])
      end
    elseif not comp(K[t], x) then
      if y == nil or comp(K[t], y) then
        coroutine.yield(K[t], V[t])
      end
    end
    if y == nil or comp(K[t], y) then
      return each(self, x, y, R[t])
    end
  end
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.tree" }

function class:insert(k, v)
  local root, ok, t = insert(self, k, self.root, false, 0)
  self.root = root
  self.V[t] = v
  return ok
end

function class:delete(k)
  local root, ok, t = delete(self, k, self.root, false, 0, 0)
  self.root = root
  if ok then
    dispose(self, t)
  end
  return ok
end

function class:find(k)
  return find(self, k)
end

function class:next(k)
  return next(self, k, self.root)
end

function class:each(lower_bound, upper_bound)
  return coroutine.wrap(function (self, t)
    return each(self, lower_bound, upper_bound, t)
  end), self, self.root
end

return setmetatable(class, {
  __call = function (_, comp)
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
})
