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
require "std-string"

--------------------------------------------------------------------------------

local __errno_table = nil

function __errno_to_string(errno)
  if __errno_table == nil then
    __errno_table = {
      "E2BIG: Argument list too long.";
      "EACCES: Permission denied.";
      "EADDRINUSE: Address in use.";
      "EADDRNOTAVAIL: Address not available.";
      "EAFNOSUPPORT: Address family not supported.";
      "EAGAIN: Resource unavailable, or operation would block.";
      "EALREADY: Connection already in progress.";
      "EBADF: Bad file descriptor.";
      "EBADMSG: Bad message.";
      "EBUSY: Device or resource busy.";
      "ECANCELED: Operation canceled.";
      "ECHILD: No child processes.";
      "ECONNABORTED: Connection aborted.";
      "ECONNREFUSED: Connection refused.";
      "ECONNRESET: Connection reset.";
      "EDEADLK: Resource deadlock would occur.";
      "EDESTADDRREQ: Destination address required.";
      "EDOM: Mathematics argument out of domain of function.";
      "EDQUOT: Reserved.";
      "EEXIST: File exists.";
      "EFAULT: Bad address.";
      "EFBIG: File too large.";
      "EHOSTUNREACH: Host is unreachable.";
      "EIDRM: Identifier removed.";
      "EILSEQ: Illegal byte sequence.";
      "EINPROGRESS: Operation in progress.";
      "EINTR: Interrupted function.";
      "EINVAL: Invalid argument.";
      "EIO: I/O error.";
      "EISCONN: Socket is connected.";
      "EISDIR: Is a directory.";
      "ELOOP: Too many levels of symbolic links.";
      "EMFILE: File descriptor value too large.";
      "EMLINK: Too many links.";
      "EMSGSIZE: Message too large.";
      "EMULTIHOP: Reserved.";
      "ENAMETOOLONG: Filename too long.";
      "ENETDOWN: Network is down.";
      "ENETRESET: Connection aborted by network.";
      "ENETUNREACH: Network unreachable.";
      "ENFILE: Too many files open in system.";
      "ENOBUFS: No buffer space available.";
      "ENODEV: No such device.";
      "ENOENT: No such file or directory.";
      "ENOEXEC: Executable file format error.";
      "ENOLCK: No locks available.";
      "ENOLINK: Reserved.";
      "ENOMEM: Not enough space.";
      "ENOMSG: No message of the desired type.";
      "ENOPROTOOPT: Protocol not available.";
      "ENOSPC: No space left on device.";
      "ENOSYS: Function not supported.";
      "ENOTCONN: The socket is not connected.";
      "ENOTDIR: Not a directory or a symbolic link to a directory.";
      "ENOTEMPTY: Directory not empty.";
      "ENOTRECOVERABLE: State not recoverable.";
      "ENOTSOCK: Not a socket.";
      "ENOTSUP: Not supported, or operation not supported on socket.";
      "ENOTTY: Inappropriate I/O control operation.";
      "ENXIO: No such device or address.";
      "EOVERFLOW: Value too large to be stored in data type.";
      "EOWNERDEAD: Previous owner died.";
      "EPERM: Operation not permitted.";
      "EPIPE: Broken pipe.";
      "EPROTO: Protocol error.";
      "EPROTONOSUPPORT: Protocol not supported.";
      "EPROTOTYPE: Protocol wrong type for socket.";
      "ERANGE: Result too large.";
      "EROFS: Read-only file system.";
      "ESPIPE: Invalid seek.";
      "ESRCH: No such process.";
      "ESTALE: Reserved.";
      "ETIMEDOUT: Connection timed out.";
      "ETXTBSY: Text file busy.";
      "EXDEV: Cross-device link.";
      "ENOTCAPABLE: Extension: Capabilities insufficient.";
    }
  end

  local s = __errno_table[errno]
  if s == nil then
    return "unknown error: "..integer_to_string(errno)
  else
    return s
  end
end

local __atcwd = -1

function __get_atcwd()
  if __atcwd ~= -1 then
    return __atcwd
  end

  local fd = 3
  local prestat = __new(8)
  while true do
    local errno = __fd_prestat_get(fd, prestat)
    if errno == 0 then
      local tag = __i32_load(prestat)
      if tag == 0 then
        -- wasmer:   NULを含む
        -- wasmtime: NULを含まない
        local size = __i32_load(prestat + 4)
        local data = __new(size + 1)
        local errno = __fd_prestat_dir_name(fd, data, size)
        if errno == 0 then
          __i32_store8(data + size, 0x00)
          local dir = __pack_string(__cstring_size(data), data)
          if string_compare(dir, ".") == 0 then
            __atcwd = fd
            break
          end
        end
      end
    else
      break
    end
    fd = fd + 1
  end

  return __atcwd
end

function __read_all_impl(fd)
  local item = __new(8)
  local out = __new(4)
  local result = ""

  while true do
    local n = 4096
    local data = __new(n)
    __i32_store(item, data)
    __i32_store(item + 4, n - 1)
    local errno = __fd_read(fd, item, 1, out)
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

function __write_string_impl(fd, s)
  local size, data = __unpack_string(s)
  local item = __new(8)
  __i32_store(item, data)
  __i32_store(item + 4, size)
  local out = __new(4)
  __i32_store(out, 0)
  __fd_write(fd, item, 1, out)
end

function error(message)
  __write_string_impl(2, message)
  __write_string_impl(2, "\n")
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

-- rights
-- 1<<0 = 0x01: fd_datasync
-- 1<<1 = 0x02: fd_read
-- 1<<2 = 0x04: fd_seek (implies fd_tell)
-- 1<<3 = 0x08: fd_fdstat_set_flags
-- 1<<4 = 0x10: fd_sync
-- 1<<5 = 0x20: fd_tell
-- 1<<6 = 0x40: fd_write
--  :

function io_open_read(path)
  local size, data = __unpack_string(path)
  local fd = __new(4)
  local errno = __path_open(
    __get_atcwd(),     -- dirfd
    0,                 -- dirflags
    data,              -- path
    size,              -- path_len
    0,                 -- o_flags
    __i64_const(0x26), -- fs_rights_base
    __i64_const(0x00), -- fs_rights_inheriting
    0,                 -- fs_flags
    fd)                -- fd
  if errno == 0 then
    return true, __i32_load(fd)
  else
    return false, __errno_to_string(errno)
  end
end

function io_open_write(path)
  local size, data = __unpack_string(path)
  local fd = __new(4)
  local errno = __path_open(
    __get_atcwd(),     -- dirfd
    0,                 -- dirflags
    data,              -- path
    size,              -- path_len
    0,                 -- o_flags
    __i64_const(0x64), -- fs_rights_base
    __i64_const(0x00), -- fs_rights_inheriting
    0,                 -- fs_flags
    fd)                -- fd
  if errno == 0 then
    return true, __i32_load(fd)
  else
    return false, __errno_to_string(errno)
  end
end

function file_close(fd)
  __fd_close(fd)
end

function file_read_all(fd)
  return __read_all_impl(fd)
end

function file_write_string(fd, s)
  __write_string_impl(fd, s)
end

function file_write_integer(fd, v)
  __write_string_impl(fd, integer_to_string(v))
end

function io_read_all()
  return __read_all_impl(0)
end

function io_write_string(s)
  __write_string_impl(1, s)
end

function io_write_integer(v)
  __write_string_impl(1, integer_to_string(v))
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
  __write_string_impl(2, "memory usage: ")
  __write_string_impl(2, integer_to_string(__heap_pointer))
  __write_string_impl(2, "\n")
end
