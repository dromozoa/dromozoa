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
local matcher = require "dromozoa.matcher"
local source_location = require "dromozoa.source_location"
local token_stream = require "dromozoa.token_stream"

---@param source string
---@param filename string
---@return dromozoa.token_stream
local function new_lexer(source, filename)
  local matcher = matcher.new(source, source_location.new(filename))
  return token_stream.new(function()
    return lexer.lex(matcher)
  end)
end

local p = new_lexer([=[
--[[1]]local--[[3]]--[[4]]x--[[6]]--[[7]]=--[[9]]--[[10]]1--[[12]]
]=], "=(test)")

p:peek():require "local"
p:read():require "local"
p:peek():require "Name"
p:read():require "Name"
p:unread()
p:peek():require "Name"
p:read():require "Name"
p:unread()
p:unread()
p:peek():require "local"
p:peek():require "local"
p:read():require "local"
p:read():require "Name"
p:peek():require "="
p:unread()
p:unread()
p:peek():require "local"
p:read():require "local"
p:read():require "Name"
p:read():require "="
p:read():require "Integer"
p:read():require "EOF"
