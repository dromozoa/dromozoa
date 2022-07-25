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
local tree = require "dromozoa.tree"

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
  return tree.next, private[self]
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

for k, v in tree.next, self do
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

local orig_pairs = pairs
local function pairs(x)
  local metatable = getmetatable(x)
  if metatable.__pairs ~= nil then
    local a, b, c = metatable.__pairs(x)
    return a, b, c
  else
    return next, x, nil
  end
end

local buffer = {}
for k, v in pairs(m) do
  buffer[#buffer + 1] = k .. "=" .. v
end
-- print(table.concat(buffer, ";"))
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")

local buffer = {}
for k, v in pairs(m) do
  buffer[#buffer + 1] = k .. "=" .. v
  m[k] = nil
end
-- print(table.concat(buffer, ";"))
assert(table.concat(buffer, ";") == "baz=3;foo=1;qux=4")
