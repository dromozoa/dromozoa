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

function __roundup(n, a)
  local r = n % a
  if r == 0 then
    return n
  else
    return n + a - r
  end
end

function __ceil_pow2(v)
  if v <= 1 then
    return 1
  else
    return 1 << 32 - __i32_clz(v - 1)
  end
end

function __new(n)
  local pointer = __heap_pointer
  __heap_pointer = pointer + __roundup(n, 8)

  local memory_size = __memory_size() * 65536
  if __heap_pointer >= memory_size then
    __memory_grow(__roundup(__heap_pointer, 65536) >> 16)
  end

  return pointer
end

function __length(p)
  return __i32_load(p)
end

function __unpack_string(s)
  local size = __i32_load(s)
  local data = __i32_load(s + 4)
  return size, data
end

function __pack_string(size, data)
  local this = __new(8)
  __i32_store(this, size)
  __i32_store(this + 4, data)
  return this
end

function __unpack_table(t)
  local size = __i32_load(t)
  local capacity = __i32_load(t + 4)
  local data = __i32_load(t + 8)
  return size, capacity, data
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

function __power(v, n)
  if n < 1 then
    return 1
  else
    local result = 1
    for i = 1, n do
      result = result * v
    end
    return result
  end
end

function __new_table(size)
  local capacity = __ceil_pow2(size)
  local data = __new(capacity * 4)
  local this = __new(12)
  __i32_store(this, size)
  __i32_store(this + 4, capacity)
  __i32_store(this + 8, data)
  return this
end

-- スタック順序に合わせて、引数の順序を入れ替える。
function __set_table(v, t, i)
  local size, capacity, data = __unpack_table(t)
  if i > capacity then
    local new_capacity = __ceil_pow2(i)
    local new_data = __new(new_capacity * 4)
    __memory_copy(new_data, data, size * 4)
    __memory_fill(new_data + size * 4, 0x00, (new_capacity - size) * 4)

    capacity = new_capacity
    data = new_data

    __i32_store(t + 4, capacity)
    __i32_store(t + 8, data)
  end

  if i > size then
    size = i
    __i32_store(t, size)
  end

  __i32_store(data + (i - 1) * 4, v)
end

function __get_table(t, i)
  local size, capacity, data = __unpack_table(t)
  if i > size then
    return nil
  else
    return __i32_load(data + (i - 1) * 4)
  end
end

function error(message)
  io_write_string_impl(2, message)
  io_write_string_impl(2, "\n")
  __unreachable()
end

function integer_to_string(v)
  local b = __new(16)
  local p = b + 15
  __i32_store8(p, 0x00)

  local neg = v < 0
  if neg then
    v = -v
  end

  repeat
    local r = v % 10
    v = v // 10
    p = p - 1
    __i32_store8(p, r + 0x30)
  until v == 0

  if neg then
    p = p - 1
    __i32_store8(p, 0x2D)
  end

  return __pack_string(b + 15 - p, p)
end

function io_read_all()
  local item = __new(8)
  local out = __new(4)
  local result = ""

  while true do
    local n = 4096
    local data = __new(n)
    __i32_store(item, data)
    __i32_store(item + 4, n - 1)
    local errno = __fd_read(0, item, 1, out)
    if errno ~= 0 then
      error("io read error: "..integer_to_string(errno))
    end
    local size = __i32_load(out)
    if size == 0 then
      break
    end
    __i32_store8(data + size, 0x00)
    result = result..__pack_string(size, data)
  end

  return result
end

function io_write_string_impl(fd, s)
  local size, data = __unpack_string(s)
  local item = __new(8)
  __i32_store(item, data)
  __i32_store(item + 4, size)
  local out = __new(4)
  __i32_store(out, 0)
  __fd_write(fd, item, 1, out)
end

function io_write_string(s)
  io_write_string_impl(1, s)
end

function io_write_integer(v)
  io_write_string(integer_to_string(v))
end

function string_byte(s, i)
  local size, data = __unpack_string(s)
  if i > size then
    return 0
  else
    return __i32_load8(data + (i - 1))
  end
end

function string_char(t)
  local size = #t
  local data = __new(size + 1)
  for i = 1, size do
    __i32_store8(data + (i - 1), t[i])
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

  -- a == bなはずだけど
  return a_size - b_size
end

function string_sub(s, i, j)
  local s_size, s_data = __unpack_string(s)

  if j > s_size then
    j = s_size
  end

  local size = j - i + 1
  local data = __new(size + 1)
  __memory_copy(data, s_data + i - 1, size)
  __i32_store8(data + size, 0x00)

  return __pack_string(size, data)
end

function table_insert(t, v)
  t[#t + 1] = v
end
