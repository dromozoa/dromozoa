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
  if t.L.N == t.N then
    t, t.L, t.L.R = t.L, t.L.R, t
  end
  return t
end

local function split(self, t)
  if t.R.R.N == t.N then
    t, t.R, t.R.L = t.R, t.R.L, t
    t.N = t.N + 1
  end
  return t
end

local function insert(self, x, t, ok, last)
  local bottom = self.bottom
  local compare = self.compare

  if t == bottom then
    self.size = self.size + 1

    t = {
      K = x;
      L = bottom;
      R = bottom;
      N = 1;
    }

    ok = true
    last = t
  else
    if compare(x, t.K) then
      t.L, ok, last = insert(self, x, t.L, ok, last)
    elseif compare(t.K, x) then
      t.R, ok, last = insert(self, x, t.R, ok, last)
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
  local bottom = self.bottom

  ok = false

  if t ~= bottom then
    local compare = self.compare

    -- 1. Search down the tree and set pointers last and deleted.
    last = t
    if compare(x, t.K) then
      t.L, ok, last, deleted = delete(self, x, t.L, ok, last, deleted)
    else
      deleted = t
      t.R, ok, last, deleted = delete(self, x, t.R, ok, last, deleted)
    end

    -- 2. At the bottom of the tree we remove the element (if it is present).
    if t == last and deleted ~= bottom and not compare(deleted.K, x) then
      deleted.K = t.K
      deleted.V = t.V
      deleted = bottom
      t = t.R
      ok = true

    -- 3. On the way back, we rebalance.
    elseif t.L.N < t.N - 1 or t.R.N < t.N - 1 then
      t.N = t.N - 1
      if t.R.N > t.N then
        t.R.N = t.N
      end
      t = skew(self, t)
      t.R = skew(self, t.R)
      t.R.R = skew(self, t.R.R)
      t = split(self, t)
      t.R = split(self, t.R)
    end
  end

  return t, ok, last, deleted
end

---------------------------------------------------------------------------

local function dispose(self, t)
  local compare = self.compare

  self.size = self.size - 1

  t.K = nil
  t.V = nil
  t.L = nil
  t.R = nil
  t.N = nil
  t = nil
end

local function find(self, x)
  local bottom = self.bottom
  local compare = self.compare

  local t = self.root
  while t ~= bottom do
    if compare(x, t.K) then
      t = t.L
    elseif compare(t.K, x) then
      t = t.R
    else
      return t.K, t.V
    end
  end
end

local function next(self, x, t, k, v)
  local bottom = self.bottom

  if t ~= bottom then
    local compare = self.compare

    if x == nil or compare(x, t.K) then
      k, v = next(self, x, t.L, k, v)
      if k == nil then
        return t.K, t.V
      else
        return k, v
      end
    end
    return next(self, x, t.R, k, v)
  end

  return k, v
end

local function each(self, x, y, t)
  local bottom = self.bottom

  if t ~= bottom then
    local compare = self.compare

    if x == nil or compare(x, t.K) then
      each(self, x, y, t.L)
      if y == nil or compare(t.K, y) then
        coroutine.yield(t.K, t.V)
        return each(self, x, y, t.R)
      end
    elseif y == nil or compare(t.K, y) then
      if not compare(t.K, x) then
        coroutine.yield(t.K, t.V)
      end
      return each(self, x, y, t.R)
    end
  end
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.tree" }

function class:insert(k, v)
  local root, ok, t = insert(self, k, self.root, false, 0)
  self.root = root
  t.V = v
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
  __call = function (_, compare)
    if compare == nil then
      compare = function (a, b) return a < b end
    end

    local bottom = {}
    bottom.L = bottom
    bottom.R = bottom
    bottom.N = 0

    return setmetatable({
      bottom = bottom;
      root = bottom;
      size = 0;
      compare = compare;
    }, metatable)
  end
})
