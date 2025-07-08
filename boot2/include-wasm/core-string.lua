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

function __cstring_size(data)
  local n = -1
  repeat
    n = n + 1
  until __i32_load8(data + n) == 0x00
  return n
end

function __pack_string(size, data)
  local s = __new(8)
  __i32_store(s, size)
  __i32_store(s + 4, data)
  return s
end

function __unpack_string(s)
  local size = __i32_load(s)
  local data = __i32_load(s + 4)
  return size, data
end

function __concat(a, b)
  local a_size, a_data = __unpack_string(a)
  local b_size, b_data = __unpack_string(b)

  local size = a_size + b_size
  local data = __new(size + 1)
  __memory_copy(data, a_data, a_size)
  __memory_copy(data + a_size, b_data, b_size)
  __i32_store8(data + size, 0x00)

  return __pack_string(size, data)
end
