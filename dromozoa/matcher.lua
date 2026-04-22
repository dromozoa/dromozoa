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

local source_location = require "dromozoa.source_location"

---@class dromozoa.matcher
---@field source string
---@field srcloc dromozoa.source_location
---@field _0 string?
---@field _1 string?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.matcher",
}

---@param source string
---@param filename string
---@return dromozoa.matcher
function class.new(source, filename)
  return setmetatable({
    source = source,
    srcloc = source_location.new(filename),
    _1 = nil,
    _2 = nil,
  }, metatable)
end

---@param pattern string
---@result boolean
function class:match(pattern)
  local i, j, value = self.source:find("^" .. pattern, self.srcloc.position)
  if i then
    local text = self.source:sub(i, j)
    self.srcloc:update(text)
    self._0 = text
    self._1 = value
    return true
  end
  self._0 = nil
  self._1 = nil
  return false
end

---@result boolean
function class:eof()
  return self.srcloc.position > #self.source
end

return class
