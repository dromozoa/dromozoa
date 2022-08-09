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

local a = 1
local a = a + a
local a = a + a

do
  local a = a + a
end

do
  local a = a + a
  do
    local a = a + a
  end
end

do
  local a = a + a
  do
    local a = a + a
    do
      local a = a + a
    end
  end
end

for i = 1, 1 do end
for i = 1, 1, 1 do end
for k, v in {}, next do end
