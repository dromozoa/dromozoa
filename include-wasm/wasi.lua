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
    return "error number "..integer_to_string(errno)
  else
    return s
  end
end

local __at_fdcwd = -1

function __get_at_fdcwd()
  if __at_fdcwd == -1 then
    local prestat = __new(8)
    local fd = 3
    while true do
      if __fd_prestat_get(fd, prestat) ~= 0 then
        break
      end
      local tag = __i32_load(prestat)
      if tag == 0 then
        -- wasmer:   NUL終端された文字列を返す
        -- wasmtime: NUL終端されていない文字列を返す
        local size = __i32_load(prestat + 4)
        local data = __new(size + 1)
        if __fd_prestat_dir_name(fd, data, size) == 0 then
          __i32_store8(data + size, 0x00)
          if string_compare(__pack_string(__cstring_size(data), data), ".") == 0 then
            __at_fdcwd = fd
            break
          end
        end
      end
      fd = fd + 1
    end
  end
  assert(__at_fdcwd ~= -1)
  return __at_fdcwd
end

local __right_fd_datasync         = 0x01
local __right_fd_read             = 0x02
local __right_fd_seek             = 0x04 -- implies __right_fd_tell
local __right_fd_fdstat_set_flags = 0x08
local __right_fd_sync             = 0x10
local __right_fd_tell             = 0x20
local __right_fd_write            = 0x40
local __right_fd_advice           = 0x80

function __open(path, rights)
  local size, data = __unpack_string(path)
  local fd = __new(4)
  local errno = __path_open(
    __get_at_fdcwd(),         -- dirfd
    0,                        -- dirflags
    data,                     -- path
    size,                     -- path_len
    0,                        -- o_flags
    __i64_extend_i32(rights), -- fs_rights_base
    __i64_const(0),           -- fs_rights_inheriting
    0,                        -- fs_flags
    fd)                       -- fd
  if errno == 0 then
    return true, __i32_load(fd)
  else
    return false, "io error: "..__errno_to_string(errno)
  end
end

function __close(fd)
  local errno = __fd_close(fd)
  if errno ~= 0 then
    error("io error: "..__errno_to_string(errno))
  end
end

function __read_all(fd)
  local size = 4096
  local data = __new(size)
  local iovs = __new(8)
  __i32_store(iovs, data)
  __i32_store(iovs + 4, size - 1)
  local nread = __new(4)
  local result = ""

  while true do
    local errno = __fd_read(fd, iovs, 1, nread)
    if errno ~= 0 then
      error("io error: "..__errno_to_string(errno))
    end
    local size = __i32_load(nread)
    if size == 0 then
      break
    end
    __i32_store8(data + size, 0x00)
    result = result..__pack_string(size, data)
  end

  return result
end

function __write_string(fd, s)
  local size, data = __unpack_string(s)
  local iovs = __new(8)
  __i32_store(iovs, data)
  __i32_store(iovs + 4, size)
  local nwrite = __new(4)
  local errno = __fd_write(fd, iovs, 1, nwrite)
  if errno ~= 0 then
    error("io error: "..__errno_to_string(errno))
  end
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

function os_exit(code)
  __proc_exit(code)
end
