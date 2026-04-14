-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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

require "runtime"

---@enum Kind
local Kind = {
  Name = 1;
}

---@class Node
---@field kind Kind
---@field value string
---@field file string
---@field position integer

---@param kind Kind
---@param value string
---@param file string
---@param position integer
---@return Node
function new_token(kind, value, file, position)
  return {
    kind = kind;
    value = value;
    file = file;
    position = position;
  }
end

---@param filename string
---@param source string
---@return Node[]
function lexer(filename, source)
  local p = 1
  local n = #source
  local result = {}

  while p <= n do
    local q = 0
    local token
  end


  result[1] = new_token(Kind.Name, "x", filename, 1)
  print(filename)
  print(source)
  return result
end

---@param filename string
function parse_file(filename)
  local source = read_file(filename)
  local tokens = lexer(filename, source)
  print(tokens)
end

function main()
  local args = get_arguments()
  parse_file(args[1])
  return 0
end

export_start(main)
