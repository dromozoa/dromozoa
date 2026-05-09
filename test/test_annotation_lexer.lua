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
local lexer = require "dromozoa.lexer"
local matcher = require "dromozoa.matcher"

---@param source string
---@return dromozoa.annotation_lexer
local function new_annotation_lexer(source)
  local tokens = lexer.new():lex(source, "=(test)")
  for _, token in ipairs(tokens) do
    if token:check "Comment" then
      local matcher = matcher.new(token.text, token.srcloc)
      if token.subkind == "Short" then
        assert(matcher:match "%-%-%-")
      elseif token.subkind == "Long" then
        assert(matcher:match "%-%-%[=*%[")
      end
      return annotation_lexer.new(matcher)
    end
  end
  error "token not found"
end

local lexer = new_annotation_lexer([[
do
  ---@type fun(x: integer):boolean, string?
  local f
end
]])

local token
token = lexer:read():require "@type"; assert(token.srcloc.line == 2 and token.srcloc.column)
token = lexer:read():require "Name"
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
