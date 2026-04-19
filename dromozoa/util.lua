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

---@param file string
---@return string
function class.read_file(file)
  local handle <close> = assert(io.open(file, "rb"))
  return handle:read "a"
end

return class
