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
    print("__close", self[1])
  end;
}

if true then
  local a <close> = setmetatable({ "a" }, metatable)
  while true do
    local b <close> = setmetatable({ "b" }, metatable)
    do
      local c <close> = setmetatable({ "c" }, metatable)
    end
    local d <close> = setmetatable({ "d" }, metatable)
    do
      local e <close> = setmetatable({ "e" }, metatable)
      break
    end
    local f <close> = setmetatable({ "f" }, metatable)
  end
  local g <close> = setmetatable({ "g" }, metatable)
end
