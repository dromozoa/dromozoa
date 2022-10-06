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
local quote_js = require "dromozoa.quote_js"

local class = {}
local metatable = { __index = class, __name = "dromozoa.compiler.source_map" }

local base64 = { [62] = "+", [63] = "/" }
for i = 0, 25 do
  base64[i] = string.char(("A"):byte() + i)
end
for i = 26, 51 do
  base64[i] = string.char(("a"):byte() + i - 26)
end
for i = 52, 61 do
  base64[i] = string.char(("0"):byte() + i - 52)
end

local function append_vlq(result, a)
  if a < 0 then
    a = 1 - a * 2
  else
    a = a * 2
  end

  while true do
    local b = a % 0x20
    a = (a - b) / 0x20
    if a > 0 then
      append(result, base64[b + 0x20])
    else
      append(result, base64[b])
      break
    end
  end
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
  append(result, '{"version":3,')
  if self.root then
    append(result, '"sourceRoot":', quote_js(self.root), ",")
  end
  append(result, '"sources":[')
  for i, file in ipairs(self.files) do
    if i > 1 then
      append(result, ",")
    end
    append(result, quote_js(file))
  end
  append(result, '],"names":[],"mappings":"')

  local prev_file = 0
  local prev = {}

  for _, mapping in ipairs(self) do
    local file = mapping.file
    local line = mapping.line
    local column = mapping.column

    local prev_line = 0
    local prev_column = 0
    local p = prev[file]
    if p then
      prev_line = p.line
      prev_column = p.column
    else
      p = { line = 0, column = 0 }
      prev[file] = p
    end

    local f = file - prev_file
    local n = line - prev_line
    local c = column - prev_column
    if f == 0 and n == 0 and c == 0 then
      append(result, ";")
    else
      append(result, "A")
      append_vlq(result, f)
      append_vlq(result, n)
      append_vlq(result, c)
      append(result, ";")
      prev_file = file
      p.line = line
      p.column = column
    end
  end

  append(result, '"}\n')
  return result
end

return setmetatable(class, {
  __call = function (_, root)
    return setmetatable({ root = root, files = {} }, metatable)
  end;
})
