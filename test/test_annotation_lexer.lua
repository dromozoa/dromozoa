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
assert(token.start_srcloc.line == 2 and token.start_srcloc.column == 4)
token = lexer:read():require "Name"
assert(token.start_srcloc.line == 2 and token.start_srcloc.column == 10)
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

local lexer = new_annotation_lexer "-1 2a 3. 4.5 6.7.8"
local token
token = lexer:read():require "Integer"
assert(token.value == -1)
token = lexer:read():require "Integer"
assert(token.value == 2)
token = lexer:read():require "Name"
assert(token.value == "a")
token = lexer:read():require "Name"
assert(token.value == "3.")
token = lexer:read():require "Name"
assert(token.value == "4.5")
token = lexer:read():require "Name"
assert(token.value == "6.7.8")

-- string, code
local lexer = new_annotation_lexer [=[
"foo\
bar\z
baz" [[
foo
barbaz]]
`` `T`
]=]
local token
token = lexer:read():require "String"
assert(token.subkind == "Short")
assert(token.value == "foo\nbarbaz")
assert(token.start_srcloc.line == 2 and token.start_srcloc.column == 4)

token = lexer:read():require "String"
assert(token.subkind == "Long")
assert(token.value == "foo\nbarbaz")
assert(token.start_srcloc.line == 4 and token.start_srcloc.column == 6)

token = lexer:read():require "Code"
assert(token.value == "")
assert(token.start_srcloc.line == 7 and token.start_srcloc.column == 1)

token = lexer:read():require "Code"
assert(token.value == "T")
assert(token.start_srcloc.line == 7 and token.start_srcloc.column == 4)

local lexer = new_annotation_lexer "@comment"
lexer:read():require "EOF"
assert(#lexer.tokens == 2)
local token = lexer.tokens[1]
assert(token.subkind == "@")
assert(token.value == "comment")

local lexer = new_annotation_lexer "@private @comment"
lexer:read():require "@private"
lexer:read():require "EOF"
assert(#lexer.tokens == 4)
local token = lexer.tokens[3]
assert(token.subkind == "@")
assert(token.value == "comment")
