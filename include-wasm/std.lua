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

function assert(condition)
  if not condition then
    error "assertion failed!"
  end
end

function error(message)
  io_stderr_write_string(message)
  io_stderr_write_string "\n"
  __unreachable()
end

function get_args()
  local sizes = __new(8)
  __args_sizes_get(sizes, sizes + 4)

  local args_size = __i32_load(sizes)
  local args_data = __new(args_size * 4)
  local buffer_size = __i32_load(sizes + 4)
  local buffer_data = __new(buffer_size)
  __args_get(args_data, buffer_data)

  local args = {}
  for i = 1, args_size - 1 do
    local data = __i32_load(args_data + i * 4)
    table_insert(args, __pack_string(__cstring_size(data), data))
  end
  return args
end

function show_memory_usage()
  io_stderr_write_string "memory usage: "
  io_stderr_write_string(integer_to_string(__heap_pointer))
  io_stderr_write_string "\n"
end
