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

---@class dromozoa.util
local class = {}

---@param source string
---@return string
function class.normalize_eol(source)
  return (source:gsub("\n\r", "\n"):gsub("\r\n?", "\n"))
end

---@param filename string
---@return string
function class.read_file(filename)
  local handle <close> = assert(io.open(filename, "rb"))
  return handle:read "a"
end

---@param source table
---@return table
function class.clone(source)
  local result = {}
  for index, value in next, source, nil do
    rawset(
      result,
      type(index) == "table" and class.clone(index) or index,
      type(value) == "table" and class.clone(value) or value)
  end
  return setmetatable(result, getmetatable(source))
end

return class
