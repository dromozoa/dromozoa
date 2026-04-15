-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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

require "std-string"

---@param a string
---@param b string
---@return integer
function string_compare(a, b)
  local m = string_len(a)
  local n = string_len(b)

  local min = m
  if min > n then
    min = n
  end

  for i = 1, min do
    local x = string_byte(a, i)
    local y = string_byte(b, i)
    if x ~= y then
      return x - y
    end
  end

  if m == n then
    return 0
  elseif m < n then
    return -1
  else
    return 1
  end
end
