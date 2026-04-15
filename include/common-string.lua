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

require "common-integer"
require "std-string"

---@param a string
---@param b string
---@return integer
function string_compare(a, b)
  local m = string_len(a)
  local n = string_len(b)

  for i = 1, integer_min(m, n) do
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

---@param s string
---@param search string
---@param position integer
---@return boolean
function string_starts_with(s, search, position)
  local n = string_len(s)
  for i = 1, string_len(search) do
    local j = position + i - 1
    if j > n then
      return false
    end
    local x = string_byte(s, j)
    local y = string_byte(search, i)
    if x ~= y then
      return false
    end
  end
  return true
end

---@param s string
---@param search string
---@param position integer
---@return integer
function string_find(s, search, position)
  for i = position, string_len(s) - string_len(search) + 1 do
    if string_starts_with(s, search, i) then
      return i
    end
  end
  return 0
end
