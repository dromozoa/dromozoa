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

function __check_result(n, ...)
  assert(select("#", ...) == n)
  return ...
end

function __call_indirect0(f, ...) return __check_result(0, f(...)) end
function __call_indirect1(f, ...) return __check_result(1, f(...)) end
function __call_indirect2(f, ...) return __check_result(2, f(...)) end
function __call_indirect3(f, ...) return __check_result(3, f(...)) end
function __export_start(f) return __check_result(0, f()) end

function get_args()
  local args = {}
  for i = 1, #arg do
    args[i] = arg[i]
  end
  return args
end

function os_exit(code)
  return os.exit(code)
end

function show_memory_usage()
  io.stderr:write("memory usage: ", collectgarbage "count" * 1024, "\n")
end

function io_open(path, mode)
  local file, message = io.open(path, mode)
  if file then
    return true, file
  else
    return false, message
  end
end

function io_read_all()
  return io.read "a"
end

function io_write_string(s)
  assert(type(s) == "string")
  io.write(s)
end

function io_write_integer(v)
  assert(math.type(v) == "integer")
  io.write(v)
end

function io_stderr_write_string(s)
  assert(type(s) == "string")
  io.stderr:write(s)
end

function io_stderr_write_integer(v)
  assert(math.type(v) == "integer")
  io.stderr:write(v)
end

function file_close(file)
  io.close(file)
end

function file_read_all(file)
  return file:read "a"
end

function file_write_string(file, s)
  assert(type(s) == "string")
  file:write(s)
end

function file_write_integer(file, v)
  assert(math.type(v) == "integer")
  file:write(v)
end

function integer_to_string(v)
  assert(math.type(v) == "integer")
  return tostring(v)
end

function string_byte(s, i)
  return string.byte(s, i)
end

function string_char(t)
  return string.char(table.unpack(t))
end

function string_compare(a, b)
  if a == b then
    return 0
  elseif a < b then
    return -1
  else
    return 1
  end
end

function string_sub(s, i, j)
  return string.sub(s, i, j)
end

function table_insert(t, v)
  table.insert(t, v)
end
