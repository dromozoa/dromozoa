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

function __new_table(size)
  local capacity = __ceil_pow2(size)
  local t = __new(12)
  __i32_store(t, size)
  __i32_store(t + 4, capacity)
  __i32_store(t + 8, __new(capacity * 4))
  return t
end

function __unpack_table(t)
  local size = __i32_load(t)
  local capacity = __i32_load(t + 4)
  local data = __i32_load(t + 8)
  return size, capacity, data
end

function __binary_search(t, k)
  local i = 1
  local n = #t
  while n > 0 do
    local step = n >> 1
    local m = i + step
    local r = string_compare(t[m][1], k)
    if r == 0 then
      return m
    elseif r < 0 then
      i = m + 1
      n = n - step - 1
    else
      n = step
    end
  end
  return 0
end

-- テーブルコンストラクタのスタック順序に合わせるため、直感と異なる引数順にしている
function __set_index(v, t, i)
  local size, capacity, data = __unpack_table(t)

  if i > capacity then
    capacity = __ceil_pow2(i)

    local m = capacity * 4
    local n = size * 4

    local new_data = __new(m)
    __memory_copy(new_data, data, n)
    __memory_fill(new_data + n, 0x00, m - n)
    data = new_data

    __i32_store(t + 4, capacity)
    __i32_store(t + 8, data)
  end

  if i > size then
    __i32_store(t, i)
  end

  __i32_store(data + (i - 1) * 4, v)
end

function __get_index(t, i)
  local size, capacity, data = __unpack_table(t)
  if i > size then
    return nil
  else
    return __i32_load(data + (i - 1) * 4)
  end
end

function __set_table(v, t, k)
  local i = __binary_search(t, k)
  assert(i > 0)
  t[i][2] = v
end

function __get_table(t, k)
  local i = __binary_search(t, k)
  assert(i > 0)
  return t[i][2]
end
