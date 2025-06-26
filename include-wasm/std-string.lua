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

function string_byte(s, i)
  local size, data = __unpack_string(s)
  if i > size then
    error "out of bounds"
  else
    return __i32_load8(data + i - 1)
  end
end

function string_char(t)
  local size = #t
  local data = __new(size + 1)
  for i = 1, size do
    __i32_store8(data + i - 1, t[i])
  end
  __i32_store8(data + size, 0x00)
  return __pack_string(size, data)
end

function string_compare(a, b)
  local a_size, a_data = __unpack_string(a)
  local b_size, b_data = __unpack_string(b)

  local size = a_size
  if size > b_size then
    size = b_size
  end

  for i = 0, size do
    local a_byte = __i32_load8(a_data + i)
    local b_byte = __i32_load8(b_data + i)
    if a_byte ~= b_byte then
      return a_byte - b_byte
    end
  end

  if a_size ~= b_size then
    error "invalid size"
  end
  return 0
end
