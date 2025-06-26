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

function errno_to_string(errno)
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



