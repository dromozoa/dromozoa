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

---------------------------------------------------------------------------

local function insert(t, k, v)
  local ok, i = t:insert(k)
  if ok then
    t.V[i] = v
  end
  return ok
end

local t = tree()
for i = 1, 256 do
  assert(insert(t, i, i * 2))
end
assert(t.size == 256)

local i = 0
for k, v, j in t:each() do
  i = i + 1
  assert(k == i)
  assert(v == i * 2)
  assert(j == i)
end
assert(i == 256)

local i = 0
for k, v, j in t.next, t do
  i = i + 1
  assert(k == i)
  assert(v == i * 2)
  assert(j == i)
end
assert(i == 256)

local i = 0
for k, v, j in t.next, t do
  i = i + 1
  assert(k == i)
  assert(v == i * 2)
  -- 削除により、jは不定になる
  assert(j <= 256)
  if k % 2 == 1 then
    local _, k, v = assert(t:delete(k))
    assert(k == i)
    assert(v == i * 2)
  end
end
assert(i == 256)
assert(t.size == 128)

for i = 1, 256 do
  local k, v, j = t:find(i)
  if i % 2 == 1 then
    assert(k == nil)
    assert(v == nil)
    assert(j == nil)
  else
    assert(k == i)
    assert(v == i * 2)
    assert(j <= 128)
  end
end

---------------------------------------------------------------------------

local i = 0
for k, v in t:each(nil, 6) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 4)

local i = 0
for k, v in t:each(nil, 5) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 4)

local i = 60
for k, v in t:each(61, 65) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 64)

local i = 124
for k, v in t:each(126, 130) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 128)

local i = 188
for k, v in t:each(189, 193) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 192)

local i = 252
for k, v in t:each(253) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 256)

local i = 252
for k, v in t:each(254) do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 256)

---------------------------------------------------------------------------

local i = 0
for k, v, j in t.next, t do
  i = i + 2
  assert(k == i)
  assert(v == i * 2)
  assert(j <= 256)
  local _, k, v = assert(t:delete(k))
  assert(k == i)
  assert(v == i * 2)
end
assert(i == 256)
assert(t.size == 0)
