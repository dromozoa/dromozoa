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

local annotation_lex = require "dromozoa.annotation_lex"
local matcher = require "dromozoa.matcher"
local source_location = require "dromozoa.source_location"
local token_stream = require "dromozoa.token_stream"

---@param source string
---@return dromozoa.token_stream
local function new_annotation_lexer(source)
  local srcloc = source_location.new "=(test)"
  srcloc.line = 2
  srcloc.column = 4
  return token_stream.new(annotation_lex, matcher.new(source, srcloc))
end

local lexer = new_annotation_lexer "@type fun(x: integer):boolean, string?"

local token
token = lexer:read():require "@type"
assert(token.srcloc.line == 2 and token.srcloc.column == 4)
token = lexer:read():require "Name"
assert(token.srcloc.line == 2 and token.srcloc.column == 10)
token = lexer:read():require "("
token = lexer:read():require "Name"
token = lexer:read():require ":"
token = lexer:read():require "Name"
token = lexer:read():require ")"
token = lexer:read():require ":"
token = lexer:read():require "Name"
token = lexer:read():require ","
token = lexer:read():require "Name"
token = lexer:read():require "?"
token = lexer:read():require "EOF"
