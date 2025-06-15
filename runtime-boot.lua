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

function call_indirect0(f, ...) return f(...) end
function call_indirect1(f, ...) return f(...) end
function call_indirect2(f, ...) return f(...) end
function call_indirect3(f, ...) return f(...) end

function export_start(f)
  f()
end

function integer_to_string(v)
  return tostring(v)
end

function io_read_all()
  return io.read("a")
end

function io_write_integer(v)
  io.write(v)
end

function io_write_string(s)
  io.write(s)
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
