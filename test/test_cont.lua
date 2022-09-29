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

local action = "fcall() fret()"

local s = " " .. action .. " "
local actions = {}
while #s > 0 do
  local _, p = s:find "[^%w_]fcall%s*%b()%s*"
  actions[#actions + 1] = s:sub(1, p)
  if p then
    s = s:sub(p + 1)
  else
    break
  end
end

for i = 1, #actions do
  io.write("[[", actions[i], "]]\n")
end
