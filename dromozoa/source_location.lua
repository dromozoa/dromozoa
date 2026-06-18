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

---@class dromozoa.source_location
---@field filename string
---@field position integer
---@field line integer
---@field column integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.source_location",
}

---@param filename string
---@param position integer?
---@param line integer?
---@param column integer?
---@return dromozoa.source_location
function class.new(filename, position, line, column)
  return setmetatable({
    filename = filename,
    position = position or 1,
    line = line or 1,
    column = column or 1,
  }, metatable)
end

---@param self dromozoa.source_location?
---@return string
function class.to_string(self)
  if self then
    return self.filename .. ":" .. self.line .. ":" .. self.column
  else
    return "=(unknown):0:0"
  end
end

---@param that dromozoa.source_location
---@return integer
function class:compare(that)
  if self.filename == that.filename then
    return self.position - that.position
  elseif self.filename < that.filename then
    return -1
  else
    return 1
  end
end

return class
