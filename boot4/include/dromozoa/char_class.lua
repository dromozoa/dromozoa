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

require "dromozoa.runtime.string"

---@alias char_class integer[]
---@return char_class
function char_class_new()
  return { 0, 0, 0, 0, 0, 0, 0, 0 }
end

---@param char_class char_class
---@param byte integer
function char_class_set(char_class, byte)
  local i = (byte >> 5) + 1
  local j = byte & 0x1F
  char_class[i] = char_class[i] | 1 << j
end

---@param char_class char_class
---@param byte integer
---@return boolean
function char_class_test(char_class, byte)
  local i = (byte >> 5) + 1
  local j = byte & 0x1F
  return char_class[i] & 1 << j ~= 0
end
