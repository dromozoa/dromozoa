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

---@class pattern
---@field char_class char_class?
---@field literal string?
---@field m integer
---@field n integer

---constructor
---@param char_class char_class? 文字クラス
---@param literal string? リテラル
---@param m integer min
---@param n integer max
---@return pattern
function pattern_new(char_class, literal, m, n)
  return {
    char_class = char_class;
    literal = literal;
    m = m;
    n = n;
  }
end

---@param char_class char_class
---@param range string
---@return integer[]
function pattern_range(char_class, range)
  for i = 1, string_len(range), 2 do
    local x = string_byte(range, i)
    local y = string_byte(range, i + 1)
    for j = x, y do
      char_class_set(char_class, j)
    end
  end
  return char_class
end

---@param char_class char_class
---@param set string
---@return integer[]
function pattern_set(char_class, set)
  for i = 1, string_len(set) do
    char_class_set(char_class, string_byte(set, i))
  end
  return char_class
end

---@param char_class char_class
---@return char_class
function pattern_negate(char_class)
  for i = 1, 8 do
    char_class[i] = char_class[i] ~ 0xFFFFFFFF
  end
  return char_class
end

---@param char_class char_class
---@param m integer
---@param n integer
---@return pattern
function pattern_repeat(char_class, m, n)
  return pattern_new(char_class, nil, m, n)
end

---@param char_class char_class
---@return pattern
function pattern_optional(char_class)
  return pattern_repeat(char_class, 0, 1)
end
