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

local class = {}
local metatable = { __index = class }

local function construct(...)
  return setmetatable({...}, metatable)
end

local set = {}
for byte = 0x00, 0xFF do
  set[byte] = true
end
local any = construct("[", set)

function class.pattern(that)
  local t = type(that)
  if t == "number" then
    if that == 1 then
      return any
    else
    end

  elseif t == "string" then
  else
    return that
  end
end

function class.range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for j = a, b do
      set[j] = true
    end
  end
  return setmetatable({ "[", set }, metatable)
end

function class.set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return setmetatable({ "[", set }, metatable)
end

function metatable:__mul(that)
  local self = class(self)
  local that = class(that)
  return setmetatable({ ".", self, that }, metatable)
end

return setmetatable(class, {
})
