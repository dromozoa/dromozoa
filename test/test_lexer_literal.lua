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

local lua_lexer = require "dromozoa.lua_lexer"
local util = require "dromozoa.util"

local expect = require "case_lexer_literal"

local filename = "case_lexer_literal.lua"
local lexer = lua_lexer.new(filename, util.read_file(filename))
local tokens = lexer:lex()

local i = 0
for _, token in ipairs(tokens) do
  if token.kind == "Integer" or token.kind == "Float" then
    i = i + 1
    local u = token.value
    local v = expect[i]
    assert(math.type(u) == math.type(v))
    assert(u == v)
  elseif token.kind == "String" then
    i = i + 1
    local u = token.value
    local v = expect[i]
    assert(u == v)
  end
end
