-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local append = require "dromozoa.append"
local quote = require "dromozoa.compiler.quote"

local base64_encoder = { [62] = "+", [63] = "/" }
for i = 0, 25 do
  base64_encoder[i] = string.char(string.byte "A" + i)
end
for i = 26, 51 do
  base64_encoder[i] = string.char(string.byte "a" + i - 26)
end
for i = 52, 61 do
  base64_encoder[i] = string.char(string.byte "0" + i - 52)
end

local function vlq(u)
  local result = {}

  if u < 0 then
    u = 1 - u * 2
  else
    u = u * 2
  end

  while true do
    local v = u % 0x20
    u = (u - v) / 0x20
    if u > 0 then
      append(result, base64_encoder[v + 0x20])
    else
      append(result, base64_encoder[v])
      break
    end
  end

  return table.concat(result)
end

return function (source_map, result, filename)
  append(result, [[{"version":3,"file":]], quote(filename), [[,"sources":[]])

  for i, file in ipairs(source_map.files) do
    if i > 1 then
      append(result, ",")
    end
    append(result, quote(file))
  end
  append(result, [[],"names":[],"mappings":"]])

  local prev_file = 0
  local prev_line = 0
  local prev_column = 0
  for _, mapping in ipairs(source_map) do
    local file = mapping.file
    local line = mapping.line
    local column = mapping.column
    local f = file - prev_file
    local n = line - prev_line
    local c = column - prev_column
    if f == 0 and n == 0 and c == 0 then
      append(result, ";")
    else
      append(result, "A", vlq(f), vlq(n), vlq(c), ";")
      pref_file = file
      prev_line = line
      prev_column = column
    end
  end

  append(result, '"}\n')
end
