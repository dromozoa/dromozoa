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

---@param filename string
---@return string
local function read_file(filename)
  local handle <close> = assert(io.open(filename, "rb"))
  return handle:read "a"
    :gsub("\n\r", "\n")
    :gsub("\r\n?", "\n")
end

---@class lua_lexer
---@field filename string
---@field source string
local lua_lexer = {}
local lua_lexer_metatable = { __index = lua_lexer }

---@param filename string
---@param source string
---@return lua_lexer
function lua_lexer.new(filename, source)
  return setmetatable({
    filename = filename;
    source = source;
  }, lua_lexer_metatable)
end;

function lua_lexer:lex()
  print(#self.source)
end

local filename = "boot.lua"
local lexer = lua_lexer.new(filename, read_file(filename))
lexer:lex()
