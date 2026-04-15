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

---@alias char_class integer[]

---@return char_class
function char_class_new()
  return { 0, 0, 0, 0, 0, 0, 0, 0 }
end

---@param self char_class
---@param byte integer
function char_class_set(self, byte)
  local i = (byte >> 5) + 1
  local j = byte & 0x1F
  self[i] = self[i] | 1 << j
end

---@param self char_class
---@param byte integer
---@return boolean
function char_class_test(self, byte)
  local i = (byte >> 5) + 1
  local j = byte & 0x1F
  return self[i] & 1 << j ~= 0
end

---@param self char_class
---@param range string
---@return integer[]
function pattern_range(self, range)
  for i = 1, string_len(range), 2 do
    local x = string_byte(range, i)
    local y = string_byte(range, i + 1)
    for j = x, y do
      char_class_set(self, j)
    end
  end
  return self
end

---@param self char_class
---@param set string
---@return integer[]
function pattern_set(self, set)
  for i = 1, string_len(set) do
    char_class_set(self, string_byte(set, i))
  end
  return self
end

function pattern_negate(self)
  for i = 1, 8 do
    self[i] = self[i] ~ 0xFFFFFFFF
  end
  return self
end
