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

local class = {}
local metatable = { __index = class, __name = "dromozoa.compiler.source_map" }

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

function class:append_mapping(u)
  local file = self.files[u.f]
  if not file then
    file = append(self.files, u.f) - 1
    self.files[u.f] = file
  end
  append(self, { file = file, line = u.n - 1, column = u.c - 1 })
end

function class:append_empty_mappings(n)
  for i = 1, n do
    append(self, { file = 0, line = 0, column = 0 })
  end
end

function class:generate()
  local result = {}

  append(result, '{"version":3,"file":', quote(self.file), ',"sources":[')

  for i, file in ipairs(self.files) do
    if i > 1 then
      append(result, ",")
    end
    append(result, quote(file))
  end
  append(result, '],"names":[],"mappings":"')

  local prev_file = 0
  local prev_line = 0
  local prev_column = 0
  for _, mapping in ipairs(self) do
    local f = mapping.file - prev_file
    local n = mapping.line - prev_line
    local c = mapping.column - prev_column
    if f == 0 and n == 0 and c == 0 then
      append(result, ";")
    else
      append(result, "A", vlq(f), vlq(n), vlq(c), ";")
      pref_file = mapping.file
      prev_line = mapping.line
      prev_column = mapping.column
    end
  end

  append(result, '"}\n')

  return result
end

return setmetatable(class, {
  __call = function (_, file)
    return setmetatable({ file = file, files = {} }, metatable)
  end;
})
