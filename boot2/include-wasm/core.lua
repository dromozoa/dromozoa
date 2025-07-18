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

function __ceil_mul(v, a)
  local r = v % a
  if r == 0 then
    return v
  else
    return v + a - r
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
  __heap_pointer = pointer + __ceil_mul(n, 8)
  if __heap_pointer >= __memory_size() * 65536 then
    __memory_grow(__ceil_mul(__heap_pointer, 65536) >> 16)
  end
  return pointer
end

function __new_stack(n)
  local pointer = __stack_pointer + __ceil_mul(n, 8)
  if pointer >= __stack_end then
    error "stack overflow"
  end
  __stack_pointer = pointer
  return pointer
end

function __length(p)
  return __i32_load(p)
end
