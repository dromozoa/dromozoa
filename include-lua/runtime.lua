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

function export_start(f)
  assert(select("#", f()) == 0)
end

function io_read_file(path)
  local handle <close> = assert(io.open(path))
  return handle:read "a"
end

function io_write_string(s)
  assert(type(s) == "string")
  io.write(s)
end

function io_write_integer(v)
  assert(math.type(v) == "integer")
  io.write(v)
end

function string_byte(s, i)
  if 1 <= i and i <= #s then
    return string.byte(s, i)
  else
    return -1
  end
end

function string_char(t)
  return string.char(table.unpack(t))
end

function integer_to_string(v)
  assert(math.type(v) == "integer")
  return tostring(v)
end
