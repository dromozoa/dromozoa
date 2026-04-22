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

local lua_filename = "case_lexer_number.lua"
local exp_filename = "case_lexer_number.exp"

local lexer = lua_lexer.new(lua_filename, util.read_file(lua_filename))
local tokens = lexer:lex()

local result = ""
for _, token in ipairs(tokens) do
  if token.kind == "Integer" then
    result = result..("integer: %d\n"):format(token.value)
  elseif token.kind == "Float" then
    result = result..("float: %A\n"):format(token.value)
  end
end
local expect = util.read_file(exp_filename)

assert(result == expect)
