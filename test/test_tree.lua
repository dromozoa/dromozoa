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

--[[
  struct node {
    node* left;
    node* right;
    int   level;
    void* key;
  };

  root : empty

]]

local function skew(t)
  if t.left.level == t.level then
    local temp = t
    t = t.left
    temp.left = t.right
    t.right = temp
  end
  return t
end

local function split(t)
  if t.right.right.level == t.level then
    local temp = t
    t = t.right
    temp.right = t.left
    t.left = temp
    t.level = t.level + 1
  end
  return t
end

local function insert(root, x, t)
  if root.bottom == t then
    t = {}
    t.key = x
    t.left = root.bottom
    t.right = root.bottom
    t.level = 1
    return t
  else
    local u
    if x < t.key then
      t.left = insert(root, x, t.left)
    elseif x > t.key then
      t.right = insert(root, x, t.right)
    else
      error "already exists"
    end
    t = skew(t)
    t = split(t)
    return t
  end
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
local u1 = insert(root, 1, root.bottom)
io.write "----\n"
dump(root, u1)
local u2 = insert(root, 2, u1)
io.write "----\n"
dump(root, u2)
local u3 = insert(root, 3, u2)
io.write "----\n"
dump(root, u3)
local u4 = insert(root, 4, u3)
io.write "----\n"
dump(root, u4)

io.write "====\n"

local root = tree()
local u1 = insert(root, 4, root.bottom)
io.write "----\n"
dump(root, u1)
local u2 = insert(root, 3, u1)
io.write "----\n"
dump(root, u2)
local u3 = insert(root, 2, u2)
io.write "----\n"
dump(root, u3)
local u4 = insert(root, 1, u3)
io.write "----\n"
dump(root, u4)

