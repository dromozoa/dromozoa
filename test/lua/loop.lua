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

local metatable = {
  __close = function (self)
    print("__close", self, self.v)
    self.v = 0
  end;
}

local function tbc(v)
  return setmetatable({v=v}, metatable)
end

local function p(...)
  print("p", ...)
  return ...
end

local i = 0
print "while start"
while p(i <= 4) do
  local tbc1 <close> = tbc(1)
  local tbc2 <close> = tbc(2)
  i = i + 1
end
print "while end"

local i = 0
print "repeat start"
repeat
  local tbc1 <close> = tbc(1)
  local tbc2 <close> = tbc(2)
  i = i + 1
until p(i > 4)
print "repeat end"

local function f(x, y)
  if y < 6 then
    return y + 1, y + 2, y + 3, y + 4
  end
end

print "for start"
for a, b, c, d in f, tbc(1), 0, tbc(2) do
  local tbc1 <close> = tbc(3)
  local tbc2 <close> = tbc(4)
  print(a, b, c, d)
  if a == 4 then
    -- TBCつきのgeneric forでbreakするとバグを踏む。
    -- break
  end
end
print "for end"

