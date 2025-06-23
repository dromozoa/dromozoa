-- Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

local state = 1
local data = {}

local index = 0
local code
local desc

for line in io.lines() do
  if state == 1 then
    if line:find "^%(typename %$errno" then
      state = state + 1
    end
  elseif state == 2 then
    if line:find "^%)" then
      state = state + 1
    else
      local v = line:match "^%s*;;;%s*(.*)"
      if v then
        assert(not desc)
        desc = v
      else
        local v = line:match "^%s*%$(.*)"
        if v then
          assert(not code)
          code = v
          data[index] = { code, desc }
          index = index + 1
          code = nil
          desc = nil
        end
      end
    end
  end
end

for i = 1, #data do
  local item = data[i]
  io.write(([[
"E%s: %s";
]]):format(item[1]:upper(), item[2]))
end
