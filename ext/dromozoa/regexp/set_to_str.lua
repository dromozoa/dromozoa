-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local encoder = {}
for byte = 0x00, 0xFF do
  encoder[byte] = ("\\x%02X"):format(byte)
end
for byte = 0x30, 0x39 do -- [0-9]
  encoder[byte] = string.char(byte)
end
for byte = 0x41, 0x5A do -- [A-Z]
  encoder[byte] = string.char(byte)
end
for byte = 0x61, 0x7A do -- [a-z]
  encoder[byte] = string.char(byte)
end

return function (set)
  if set[256] then
    return "%enter"
  elseif set[257] then
    return "%leave"
  end

  local n = 0
  for byte in pairs(set) do
    n = n + 1
  end

  if n == 1 then
    for byte in pairs(set) do
      return encoder[byte]
    end
  elseif n == 256 then
    return "."
  end
  local neg = n > 127

  local a = {}
  local b = {}
  local i = 0

  if not neg then
    for byte = 0x00, 0xFF do
      if set[byte] then
        if b[i] == byte - 1 then
          b[i] = byte
        else
          i = i + 1
          a[i] = byte
          b[i] = byte
        end
      end
    end
  else
    for byte = 0x00, 0xFF do
      if not set[byte] then
        if b[i] == byte - 1 then
          b[i] = byte
        else
          i = i + 1
          a[i] = byte
          b[i] = byte
        end
      end
    end
  end

  local buffer = {}
  local n = 0

  if not neg then
    n = n + 1; buffer[n] = "["
  else
    n = n + 1; buffer[n] = "[^"
  end

  for i = 1, #a do
    local a = a[i]
    local b = b[i]
    if a == b then
      n = n + 1; buffer[n] = encoder[a]
    elseif a == b - 1 then
      n = n + 1; buffer[n] = encoder[a]
      n = n + 1; buffer[n] = encoder[b]
    else
      n = n + 1; buffer[n] = encoder[a]
      n = n + 1; buffer[n] = "-"
      n = n + 1; buffer[n] = encoder[b]
    end
  end

  n = n + 1; buffer[n] = "]"
  return table.concat(buffer)
end
