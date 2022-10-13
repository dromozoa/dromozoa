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

local function f(message)
  return (message:gsub("^.-: ", ""))
end

local result, message = pcall(function ()
  for i = nil, 1 do
    print(i)
  end
end)
print(result, f(message))

local result, message = pcall(function ()
  for i = 1, "two" do
    print(i)
  end
end)
print(result, f(message))

local result, message = pcall(function ()
  for i = 1, 2, true do
    print(i)
  end
end)
print(result, f(message))

local result, message = pcall(function ()
  for i = 1, 2, {} do
    print(i)
  end
end)
print(result, f(message))

local result, message = pcall(function ()
  local zero = 0
  for i = 1, -1, zero do
    print(i)
  end
end)
print(result, f(message))

for i = 1, 4 do
  print(i)
end

for i = 4, 1, -1 do
  print(i)
end
