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

local token = require "dromozoa.token"
local source_location = require "dromozoa.source_location"

---@class dromozoa.lua_lexer
---@field filename string
---@field source string
---@field srcloc dromozoa.source_location
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.lua_lexer",
}

---@param filename string
---@param source string
---@return dromozoa.lua_lexer
function class.new(filename, source)
  return setmetatable({
    filename = filename,
    source = source,
    srcloc = source_location.new(filename),
  }, metatable)
end

---@return dromozoa.token[]
function class:lex()
  local result = {}
  table.insert(result, token.new("EOF", "EOF", self.srcloc:clone()))
  return result
end

return class
