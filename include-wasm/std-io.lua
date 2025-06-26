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

function io_open(path, mode)
  local rights = __right_fd_seek | __right_fd_tell
  if string_compare(mode, "r") == 0 or string_compare(mode, "rb") == 0 then
    rights = rights | __right_fd_read
  elseif string_compare(mode, "w") == 0 or string_compare(mode, "wb") == 0 then
    rights = rights | __right_fd_write
  else
    error "io error: invalid mode"
  end
  return __open(path, rights)
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

function io_stderr_write_string(s)
  __write_string(2, s)
end

function io_stderr_write_integer(v)
  __write_string(2, integer_to_string(v))
end

function file_close(fd)
  __close(fd)
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
