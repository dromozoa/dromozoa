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

require "core"
require "core-string"
require "core-table"
require "std"
require "std-string"
require "wasi"

--------------------------------------------------------------------------------

function io_open_read(path)
  return __open(path, __right_fd_read | __right_fd_seek | __right_fd_tell)
end

function io_open_write(path)
  return __open(path, __right_fd_write | __right_fd_seek | __right_fd_tell)
end

function file_close(fd)
  __fd_close(fd)
end

function file_read_all(fd)
  return __read_all(fd)
end

function file_write_string(fd, s)
  __write_string(fd, s)
end

function file_write_integer(fd, v)
  __write_string(fd, integer_to_string(v))
end

function io_read_all()
  return __read_all(0)
end

function io_write_string(s)
  __write_string(1, s)
end

function io_write_integer(v)
  __write_string(1, integer_to_string(v))
end

function table_insert(t, v)
  t[#t + 1] = v
end

function get_args()
  local out = __new(8)
  __args_sizes_get(out, out + 4)

  local buffer_size = __i32_load(out + 4)
  local buffer_data = __new(buffer_size)
  local args_size = __i32_load(out)
  local args_data = __new(args_size * 4)
  __args_get(args_data, buffer_data)

  local result = {}
  for i = 1, args_size - 1 do
    local data = __i32_load(args_data + i * 4)
    table_insert(result, __pack_string(__cstring_size(data), data))
  end
  return result
end

function show_memory_usage()
  __write_string(2, "memory usage: ")
  __write_string(2, integer_to_string(__heap_pointer))
  __write_string(2, "\n")
end
