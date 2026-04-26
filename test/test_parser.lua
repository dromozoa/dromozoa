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

local function dump(u, buffer)
  if #u.nodes == 0 then
    table.insert(buffer, u.kind)
  else
    table.insert(buffer, "(")
    table.insert(buffer, u.kind)
    for _, v in ipairs(u.nodes) do
      table.insert(buffer, " ")
      dump(v, buffer)
    end
    table.insert(buffer, ")")
  end
  return buffer
end

local function test_parse_exp(source, expect)
  local root = assert(parser.new():parse(lexer.new():lex(source, "=test")))
  local result = table.concat(dump(root, {}))
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
  return result
end

test_parse_exp( "1 and 2 or 3", "(or (and Integer Integer) Integer)")
test_parse_exp( "1 or 2 and 3", "(or Integer (and Integer Integer))")
test_parse_exp( "1 + 2 + 3", "(+ (+ Integer Integer) Integer)")
test_parse_exp( "1 ^ 2 ^ 3", "(^ Integer (^ Integer Integer))")
