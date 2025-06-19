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
    __memory_grow(__roundup(memory_size, 65536))
  end

  return pointer
end

function __pack_string(size, data)
  local this = __new(8)
  __i32_store(this, size)
  __i32_store(this + 4, data)
  return this
end

function __unpack_string(s)
  local size = __i32_load(s)
  local data = __i32_load(s + 4)
  return size, data
end

function __unpack_table(t)
  local size = __i32_load(t)
  local capacity = __i32_load(t + 4)
  local data = __i32_load(t + 8)
  return size, capacity, data
end

function __set_table(t, i, v)
  local size, capacity, data = __unpack_table(t)
  if i > capacity then

  end

  -- TODO 実装
  -- if i <= size then
  --   __i32_store(data + (i - 1) * 4, v)
  -- elseif i <= capacity then
  -- end

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

function io_write_string(s)
  local size, data = __unpack_string(s)
  local item = __new(8)
  __i32_store(item, data)
  __i32_store(item + 4, size)
  local out = __new(4)
  __i32_store(out, 0)
  __fd_write(1, item, 1, out)
end

function io_write_integer(v)
  io_write_string(integer_to_string(v))
end
