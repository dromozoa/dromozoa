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

local annotation_lexer = require "dromozoa.annotation_lexer"
local annotation_parser = require "dromozoa.annotation_parser"
local lexer = require "dromozoa.lexer"
local matcher = require "dromozoa.matcher"

---@param u dromozoa.node
---@param buffer string[]
local function dump_impl(u, buffer)
  local enclose = #u.nodes > 0 or not u:check "Name"
  if enclose then
    table.insert(buffer, "(")
  end

  if u:check "Name" then
    table.insert(buffer, tostring(u.token.value))
  else
    table.insert(buffer, u.kind)
  end
  for _, v in ipairs(u.nodes) do
    table.insert(buffer, " ")
    dump_impl(v, buffer)
  end

  if enclose then
    table.insert(buffer, ")")
  end
end

---@param u dromozoa.node
---@return string
local function dump(u)
  local buffer = {}
  dump_impl(u, buffer)
  return table.concat(buffer)
end

---@param source string
---@return string
local function normalize(source)
  return (source
    :gsub("^%s+%(", "(")
    :gsub("%)%s+$", ")")
    :gsub("%s+%(", " (")
    :gsub("%)%s+", ") ")
    :gsub("%s+%)", ")")
  )
end

---@param source string
---@param expect string
local function test_expression(source, expect)
  local token = lexer.new():lex(source, "=(test)")[1]:require "Comment"
  assert(token.subkind == "Short")
  local matcher = matcher.new(token.text, token.srcloc)
  matcher:match "%-%-%-"
  local p = annotation_parser.new(annotation_lexer.new(matcher))
  local root = p:parse_expression(0)
  local result = dump(root)
  local expect = normalize(expect)
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
end

test_expression(
  "--- boolean | string? | integer[]",
  "(| (| boolean (? string)) ([] integer))")
