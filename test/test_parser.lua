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

---@param u dromozoa.node
---@param buffer string[]
local function dump(u, buffer)
  local n = #u.nodes
  if n > 0 then
    table.insert(buffer, "(")
  end
  if u.kind == "Integer" or u.kind == "Name" then
    table.insert(buffer, tostring(u.token.value))
  else
    table.insert(buffer, u.kind)
  end
  for _, v in ipairs(u.nodes) do
    table.insert(buffer, " ")
    dump(v, buffer)
  end
  if n > 0 then
    table.insert(buffer, ")")
  end
  return buffer
end

---@param source string
---@param expect string
local function test_parse_exp(source, expect)
  local p = parser.new()
  p.tokens = assert(lexer.new():lex(source, "=test"))
  p.index = 1
  local root = assert(p:parse_exp(0))
  local result = table.concat(dump(root, {}))
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
end

test_parse_exp("1 and 2 or 3", "(or (and 1 2) 3)")
test_parse_exp("1 or 2 and 3", "(or 1 (and 2 3))")
test_parse_exp("1 + 2 + 3", "(+ (+ 1 2) 3)")
test_parse_exp("1 ^ 2 ^ 3", "(^ 1 (^ 2 3))")

test_parse_exp("- 1 + 2 - 3", "(- (+ (- 1) 2) 3)")
test_parse_exp("- 1 ^ 2 ^ 3", "(- (^ 1 (^ 2 3)))")
test_parse_exp("not a or b", "(or (not a) b)")
test_parse_exp("1 + 2 * 3", "(+ 1 (* 2 3))")
test_parse_exp("(1 + 2) * 3", "(* (+ 1 2) 3)")
