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

local lexer = require "dromozoa.lexer"
local parser = require "dromozoa.parser"

local function dump(node, buffer)
  if #node.nodes == 0 then
    table.insert(buffer, node.kind)
  else
    table.insert(buffer, "(")
    table.insert(buffer, node.kind)
    for _, node in ipairs(node.nodes) do
      table.insert(buffer, " ")
      dump(node, buffer)
    end
    table.insert(buffer, ")")
    return buffer
  end
end

local function test(source)
  local root = assert(parser.new():parse(lexer.new():lex(source, "=test")))
  return table.concat(dump(root, {}))
end

print(test "1 and 2 or 3")
print(test "1 or 2 and 3")
print(test "1 + 2 + 3")
print(test "1 ^ 2 ^ 3")
