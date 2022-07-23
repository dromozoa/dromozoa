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

local function skew(t)
  local temp
  if t.left.level == t.level then
    -- rotate right
    temp = t
    t = t.left          -- t            = t.left
    temp.left = t.right -- t.left       = t.left.right
    t.right = temp      -- t.left.right = t
  end
  return t
end

local function split(t)
  local temp
  if t.right.right.level == t.level then
    -- rotate left
    temp = t
    t = t.right           -- t            = t.right
    temp.right = t.left   -- t.right      = t.right.left
    t.left = temp         -- t.right.left = t
    t.level = t.level + 1 -- t.right.level += 1
  end
  return t
end

local function insert(self, x, t)
  if t == self.bottom then
    t = {}
    t.key = x
    t.left = self.bottom
    t.right = self.bottom
    t.level = 1
    -- ok = true
  else
    if x < t.key then
      t.left = insert(self, x, t.left)
    elseif x > t.key then
      t.right = insert(self, x, t.right)
    else
      -- ok = false
      error "x == t.key"
    end
    t = skew(t)
    t = split(t)
  end
  return t
end

local function delete(self, x, t)
  -- ok = false
  if t ~= self.bottom then
    -- 1. Search down the tree and set pointers last and deleted.
    self.last = t
    if x < t.key then
      t.left = delete(self, x, t.left)
    else
      self.deleted = t
      t.right = delete(self, x, t.right)
    end

    -- 2. At the bottom of the tree we remove the element (if it is present).
    if t == self.last and self.deleted ~= self.bottom and x == self.deleted.key then
      self.deleted.key = t.key
      self.deleted = self.bottom
      t = t.right
      -- dispose(last)
      self.last = nil
      -- ok = true

    -- 3. On the way back, we rebalance.
    elseif t.left.level < t.level - 1 or t.right.level < t.level - 1 then
      t.level = t.level - 1
      if t.right.level > t.level then
        t.right.level = t.level
      end
      t = skew(t)
      t.right = skew(t.right)
      t.right.right = skew(t.right.right)
      t = split(t)
      t.right = split(t.right)
    end
  end
  return t
end

local function tree()
  local bottom = {}
  bottom.level = 0
  bottom.left = bottom
  bottom.right = bottom
  return setmetatable({ bottom = bottom, deleted = bottom }, metatable)
end

local function dump(root, t, n, k)
  if k == nil then
    k = " "
  end
  if n == nil then
    n = 0
  else
    n = n + 1
  end
  io.write(("  "):rep(n), k, " ", tostring(t), " ", t.level, " / ", tostring(t.key), "\n")
  if t.left ~= root.bottom then
    dump(root, t.left, n, "L")
  end
  if t.right ~= root.bottom then
    dump(root, t.right, n, "R")
  end
end

local root = tree()
local u = root.bottom
for i = 1, 16 do
  u = insert(root, i, u)
end
dump(root, u)

io.write "----\n"

u = delete(root, 3, u)
dump(root, u)

io.write "====\n"

local root = tree()
local u = root.bottom
for i = 16, 1, -1 do
  u = insert(root, i, u)
end
dump(root, u)

io.write "----\n"

u = delete(root, 3, u)
dump(root, u)

